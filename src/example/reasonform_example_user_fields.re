open ReasonForm;

module Address = Reasonform_example_address_fields;

module Value = {
  type t = {
    username: option(string),
    lastname: option(string),
    firstname: option(string),
    age: int,
    tags: list(string),
    mainAddress: option(Address.Value.t),
    addresses: list(Address.Value.t),
  };

  let empty = {username: None, lastname: None, firstname: None, age: 18, tags: [], mainAddress: None, addresses: []};
};

type form = Form.form(Value.t);

let lastname =
  Field.createField(
    ~key="lastname",
    ~getValue=values => values.Value.lastname,
    ~setValue=(value, values) => {...values, lastname: value},
  );
let firstname =
  Field.createField(
    ~key="firstname",
    ~getValue=values => values.Value.firstname,
    ~setValue=(value, values) => {...values, firstname: value},
  );
let username =
  Field.createField(
    ~key="username",
    ~getValue=values => values.Value.username,
    ~setValue=(value, values) => {...values, username: value},
  );
let age =
  Field.createField(
    ~key="age",
    ~getValue=values => values.Value.age,
    ~setValue=(value, values) => {...values, age: value},
  );
let tags =
  FieldList.createField(
    ~key="tags",
    ~getList=values => values.Value.tags,
    ~setList=(value, values) => {...values, tags: value},
    ~createFields=
      ({wrapField}) =>
        wrapField(Field.createField(~key="root", ~getValue=values => values, ~setValue=(value, _) => value)),
  );

let addresses =
  FieldList.createField(
    ~key="addresses",
    ~getList=values => values.Value.addresses,
    ~setList=(value, values) => {...values, addresses: value},
    ~createFields=Address.wrapFields,
  );

let mainAddress =
  FieldObject.createField(
    ~key="mainAddress",
    ~getObject=values => values.Value.mainAddress,
    ~setObject=(value, values) => {...values, mainAddress: value},
    ~createFields=Address.wrapOptionFields,
  );

let onValidate = form => {
  let id = form => form;
  let values: Value.t = Form.getValues(form);
  let addError = Helper.addError;

  let form =
    form
    |> (
      switch (values.lastname) {
      | None
      | Some("") => addError(`field(lastname), "Lastname is required.")
      | _ => id
      }
    )
    |> (
      switch (values.firstname) {
      | None
      | Some("") => addError(`field(firstname), "Firstname is required.")
      | _ => id
      }
    )
    |> (
      switch (values.username) {
      | None
      | Some("") => addError(`field(username), "Username is required.")
      | Some("maarek") => addError(`field(username), "Username is already used.")
      | _ => id
      }
    )
    |> (
      switch (values.age) {
      | a when a < 18 => addError(`field(age), "You must be major.")
      | _ => id
      }
    );

  let form =
    Belt.List.size(values.addresses) < 1 ?
      addError(`list(addresses), "Must contains one address at least.", form) : form;

  let form =
    form
    |> (
      switch (values.mainAddress) {
      | None => id
      | Some(address) => Address.validate(address, mainAddress.fields)
      }
    );

  let form =
    Belt.List.mapWithIndex(values.addresses, (i, a) => (i, a))
    |> Belt.List.reduce(_, form, (form, (i, address)) => form |> Address.validate(address, addresses.getRow(i)));

  let form =
    Belt.List.mapWithIndex(values.tags, (i, a) => (i, a))
    |> Belt.List.reduce(_, form, (form, (i, tag)) =>
         form
         |> (
           switch (tag) {
           | "" => addError(`field(tags.getRow(i)), "Tag is required.")
           | "forbidden" => addError(`field(tags.getRow(i)), "Forbidden tag.")
           | _ => id
           }
         )
       );
  form;
};
let initializeForm = () => Form.initializeForm(~initialValues=Value.empty, ~onValidate, ());
