[%%debugger.chrome];
module Address = {
  type t = {
    street: string,
    city: string,
  };
  let street =
    Field.createField(
      ~key="street",
      ~getValue=values => values.street,
      ~setValue=(value, values) => {...values, street: value},
    );
  let city =
    Field.createField(
      ~key="city",
      ~getValue=values => values.city,
      ~setValue=(value, values) => {...values, city: value},
    );
  let empty = {street: "", city: ""};
};
module User = {
  type t = {
    username: option(string),
    lastname: option(string),
    firstname: option(string),
    age: int,
    tags: list(string),
    mainAddress: option(Address.t),
    addresses: list(Address.t),
  };

  type form = Form.form(t);

  let initialUser = {
    username: None,
    lastname: None,
    firstname: None,
    age: 18,
    tags: [],
    mainAddress: None,
    addresses: [],
  };
  let lastname =
    Field.createField(
      ~key="lastname",
      ~getValue=values => values.lastname,
      ~setValue=(value, values) => {...values, lastname: value},
    );
  let firstname =
    Field.createField(
      ~key="firstname",
      ~getValue=values => values.firstname,
      ~setValue=(value, values) => {...values, firstname: value},
    );
  let username =
    Field.createField(
      ~key="username",
      ~getValue=values => values.username,
      ~setValue=(value, values) => {...values, username: value},
    );
  let age =
    Field.createField(
      ~key="age",
      ~getValue=values => values.age,
      ~setValue=(value, values) => {...values, age: value},
    );
  let tags =
    FieldList.createField(
      ~key="tags",
      ~getList=values => values.tags,
      ~setList=(value, values) => {...values, tags: value},
      ~createFields=
        wrapField =>
          wrapField(
            Field.createField(
              ~key="root",
              ~getValue=values => values,
              ~setValue=(value, _) => value,
            ),
          ),
    );
  type address = {
    street: Field.t(t, string),
    city: Field.t(t, string),
  };
  let addresses =
    FieldList.createField(
      ~key="addresses",
      ~getList=(values: t) => values.addresses,
      ~setList=
        (value: list(Address.t), values: t) => {
          ...values,
          addresses: value,
        },
      ~createFields=
        wrapField => {
          street: wrapField(Address.street),
          city: wrapField(Address.city),
        },
    );

  let mainAddress =
    FieldObject.createField(
      ~key="mainAddress",
      ~getObject=values => values.mainAddress,
      ~setObject=(value, values) => {...values, mainAddress: value},
      ~createFields=
        wrapField => {
          street:
            wrapField(
              Field.wrapOptionField(
                ~field=Address.street,
                ~empty=Address.empty,
                (),
              ),
            ),
          city:
            wrapField(
              Field.wrapOptionField(
                ~field=Address.city,
                ~empty=Address.empty,
                (),
              ),
            ),
        },
    );

  let onValidate = form => {
    let id = form => form;
    let values = Form.getValues(form);

    let form =
      form
      |> (
        switch (values.lastname) {
        | None
        | Some("") => lastname.bind.addError("Lastname is required.")
        | _ => id
        }
      )
      |> (
        switch (values.firstname) {
        | None
        | Some("") => firstname.bind.addError("Firstname is required.")
        | _ => id
        }
      )
      |> (
        switch (values.username) {
        | None
        | Some("") => username.bind.addError("Username is required.")
        | Some("maarek") =>
          username.bind.addError("Username is already used.")
        | _ => id
        }
      )
      |> (
        switch (values.age) {
        | a when a < 18 => age.bind.addError("You must be major.")
        | _ => id
        }
      );

    let form =
      Belt.List.size(values.addresses) < 2 ?
        addresses.bind.addError("Must contains one address at least.", form) :
        form;

    let validateAddress = (address: Address.t, field: address, form) =>
      form
      |> (
        switch (address.street) {
        | "" => field.street.bind.addError("Street is required.")
        | "forbidden" => field.street.bind.addError("Forbidden street.")
        | _ => id
        }
      )
      |> (
        switch (address.city) {
        | "" => field.city.bind.addError("City is required.")
        | "forbidden" => field.city.bind.addError("Forbidden city.")
        | _ => id
        }
      );

    let form =
      form
      |> (
        switch (values.mainAddress) {
        | None => id
        | Some(address) => validateAddress(address, mainAddress.fields)
        }
      );

    let form =
      Belt.List.mapWithIndex(values.addresses, (i, a) => (i, a))
      |> Belt.List.reduce(_, form, (form, (i, address)) =>
           form |> validateAddress(address, addresses.getRow(i))
         );

    let form =
      Belt.List.mapWithIndex(values.tags, (i, a) => (i, a))
      |> Belt.List.reduce(_, form, (form, (i, tag)) =>
           form
           |> (
             switch (tag) {
             | "" => tags.getRow(i).bind.addError("Tag is required.")
             | "forbidden" => tags.getRow(i).bind.addError("Forbidden tag.")
             | _ => id
             }
           )
         );
    form;
  };
  let initializeForm = () =>
    Form.initializeForm(~initialValues=initialUser, ~onValidate, ());
};

module UserForm = {
  type action =
    | HandleAction(User.form => User.form)
    | SetForm(User.form);
  type state = User.form;
  let component = ReasonReact.reducerComponent("UserForm");

  let displayErrors = (form: User.form, field: Bind.t(User.t, 'b)) =>
    field.isDirty(form) ?
      <div key=(field.key ++ "-" ++ "errors")>
        (
          form
          |> field.getErrors
          |> Belt.List.map(_, ReasonReact.string)
          |> Belt.List.toArray
          |> ReasonReact.array
        )
      </div> :
      ReasonReact.null;

  let make = _children => {
    ...component,
    initialState: () =>
      Form.initializeForm(
        ~initialValues=User.initialUser,
        ~onValidate=User.onValidate,
        (),
      ),
    reducer: (action: action, state) =>
      switch (action) {
      | HandleAction(action) => ReasonReact.Update(action(state))
      | SetForm(form) => ReasonReact.Update(form)
      },
    render: ({state: form, send}) => {
      Js.log(Form.fieldsToJSON(form));
      let values: User.t = Form.getValues(form);
      let dispatch = action => send(HandleAction(action));
      <div>
        <div>
          <label> (ReasonReact.string("Nom")) </label>
          <input
            type_="text"
            value=(values.lastname |> Belt.Option.getWithDefault(_, ""))
            onFocus=(_event => dispatch(User.lastname.bind.focus))
            onBlur=(_event => dispatch(User.lastname.bind.blur))
            onChange=(
              Helper.handleDomFormChange(dispatch, User.lastname, value =>
                Some(value)
              )
            )
          />
          (displayErrors(form, User.lastname.bind))
        </div>
        <div>
          <label> (ReasonReact.string({j|Prénom|j})) </label>
          <input
            type_="text"
            value=(values.firstname |> Belt.Option.getWithDefault(_, ""))
            onFocus=(_event => dispatch(User.firstname.bind.focus))
            onBlur=(_event => dispatch(User.firstname.bind.blur))
            onChange=(
              Helper.handleDomFormChange(dispatch, User.firstname, value =>
                Some(value)
              )
            )
          />
          (displayErrors(form, User.firstname.bind))
        </div>
        <div>
          <label> (ReasonReact.string({j|Username|j})) </label>
          <input
            type_="text"
            value=(values.username |> Belt.Option.getWithDefault(_, ""))
            onFocus=(_event => dispatch(User.username.bind.focus))
            onBlur=(_event => dispatch(User.username.bind.blur))
            onChange=(
              Helper.handleDomFormChange(dispatch, User.username, value =>
                Some(value)
              )
            )
          />
          (displayErrors(form, User.username.bind))
        </div>
        <div>
          <label> (ReasonReact.string({j|Age|j})) </label>
          <input
            type_="text"
            value=(values.age |> string_of_int)
            onFocus=(_event => dispatch(User.age.bind.focus))
            onBlur=(_event => dispatch(User.age.bind.blur))
            onChange=(
              Helper.handleDomFormChange(dispatch, User.age, value =>
                try (int_of_string(value)) {
                | Failure("int_of_string") => 0
                }
              )
            )
          />
          (displayErrors(form, User.age.bind))
        </div>
        <div>
          <ul>
            (
              values.tags
              |> Belt.List.mapWithIndex(_, (i, tag) =>
                   <li key={j|$i|j}>
                     <div>
                       <input
                         type_="text"
                         value=tag
                         onFocus=(
                           _event => dispatch(User.tags.getRow(i).bind.focus)
                         )
                         onBlur=(
                           _event => dispatch(User.tags.getRow(i).bind.blur)
                         )
                         onChange=(
                           Helper.handleDomFormChange(
                             dispatch, User.tags.getRow(i), value =>
                             value
                           )
                         )
                       />
                       (displayErrors(form, User.tags.getRow(i).bind))
                     </div>
                   </li>
                 )
              |> Belt.List.toArray
              |> ReasonReact.array
            )
          </ul>
          <button
            onClick=(
              event => {
                ReactEventRe.Synthetic.preventDefault(event);
                dispatch(
                  FieldList.changeValues(
                    User.tags,
                    User.tags.push("", values),
                  ),
                );
              }
            )>
            (ReasonReact.string("Add +"))
          </button>
        </div>
        <div>
          <label> (ReasonReact.string({j|Main address|j})) </label>
          <div>
            <input
              type_="text"
              value=(
                values.mainAddress
                |. Belt.Option.map(address => address.street)
                |. Belt.Option.getWithDefault("")
              )
              onFocus=(
                _event => dispatch(User.mainAddress.fields.street.bind.focus)
              )
              onBlur=(
                _event => dispatch(User.mainAddress.fields.street.bind.blur)
              )
              onChange=(
                Helper.handleDomFormChange(
                  dispatch, User.mainAddress.fields.street, value =>
                  value
                )
              )
            />
            (displayErrors(form, User.mainAddress.fields.street.bind))
          </div>
          <div>
            <input
              type_="text"
              value=(
                values.mainAddress
                |. Belt.Option.map(address => address.city)
                |. Belt.Option.getWithDefault("")
              )
              onFocus=(
                _event => dispatch(User.mainAddress.fields.city.bind.focus)
              )
              onBlur=(
                _event => dispatch(User.mainAddress.fields.city.bind.blur)
              )
              onChange=(
                Helper.handleDomFormChange(
                  dispatch, User.mainAddress.fields.city, value =>
                  value
                )
              )
            />
            (displayErrors(form, User.mainAddress.fields.city.bind))
          </div>
        </div>
        <div>
          <ul>
            (
              values.addresses
              |> Belt.List.mapWithIndex(_, (i, address) =>
                   <li key={j|$i|j}>
                     <div>
                       <input
                         type_="text"
                         value=address.street
                         onFocus=(
                           _event =>
                             dispatch(
                               User.addresses.getRow(i).street.bind.focus,
                             )
                         )
                         onBlur=(
                           _event =>
                             dispatch(
                               User.addresses.getRow(i).street.bind.blur,
                             )
                         )
                         onChange=(
                           Helper.handleDomFormChange(
                             dispatch, User.addresses.getRow(i).street, value =>
                             value
                           )
                         )
                       />
                       (
                         displayErrors(
                           form,
                           User.addresses.getRow(i).street.bind,
                         )
                       )
                     </div>
                     <div>
                       <input
                         type_="text"
                         value=address.city
                         onFocus=(
                           _event =>
                             dispatch(
                               User.addresses.getRow(i).city.bind.focus,
                             )
                         )
                         onBlur=(
                           _event =>
                             dispatch(
                               User.addresses.getRow(i).city.bind.blur,
                             )
                         )
                         onChange=(
                           Helper.handleDomFormChange(
                             dispatch, User.addresses.getRow(i).city, value =>
                             value
                           )
                         )
                       />
                       (
                         displayErrors(
                           form,
                           User.addresses.getRow(i).city.bind,
                         )
                       )
                     </div>
                   </li>
                 )
              |> Belt.List.toArray
              |> ReasonReact.array
            )
          </ul>
          (displayErrors(form, User.addresses.bind))
          <button
            onClick=(
              event => {
                ReactEventRe.Synthetic.preventDefault(event);
                dispatch(
                  FieldList.changeValues(
                    User.addresses,
                    User.addresses.push({street: "", city: ""}, values),
                  ),
                );
              }
            )>
            (ReasonReact.string("Add +"))
          </button>
        </div>
        <div>
          (
            ReasonReact.string(
              Form.formHasError(form) ? "is invalid" : "is valid",
            )
          )
        </div>
      </div>;
    },
  };
};

let component = ReasonReact.statelessComponent("App");
let make = _children => {...component, render: _ => <UserForm />};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make([||])), "app");