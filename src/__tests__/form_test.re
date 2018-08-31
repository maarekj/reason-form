open Jest;

module Address = {
  type t = {
    numero: int,
    street: string,
    city: string,
  };
  let numero =
    Field.createField(
      ~key="numero",
      ~getValue=values => values.numero,
      ~setValue=(value, values) => {...values, numero: value},
    );
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
  let empty = {numero: 0, street: "", city: ""};
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
        ({wrapField}) =>
          wrapField(Field.createField(~key="root", ~getValue=values => values, ~setValue=(value, _) => value)),
    );
  type address = {
    numero: Field.t(t, int),
    street: Field.t(t, string),
    city: Field.t(t, string),
  };
  let addresses =
    FieldList.createField(
      ~key="addresses",
      ~getList=(values: t) => values.addresses,
      ~setList=(value: list(Address.t), values: t) => {...values, addresses: value},
      ~createFields=
        ({wrapField}) => {
          numero: wrapField(Address.numero),
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
        ({wrapField}) => {
          numero: wrapField(Field.wrapOptionField(~field=Address.numero, ~empty=Address.empty, ())),
          street: wrapField(Field.wrapOptionField(~field=Address.street, ~empty=Address.empty, ())),
          city: wrapField(Field.wrapOptionField(~field=Address.city, ~empty=Address.empty, ())),
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
        | Some("maarek") => username.bind.addError("Username is already used.")
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
        addresses.bind.addError("Must contains one address at least.", form) : form;

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
      |> Belt.List.reduce(_, form, (form, (i, address)) => form |> validateAddress(address, addresses.getRow(i)));

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
  let initializeForm = () => Form.initializeForm(~initialValues=initialUser, ~onValidate, ());
};

let changeValue = (field: Field.t('a, 'b), value: 'b, form: Form.form('a)) => {
  let values = Form.getValues(form);
  let newValues = field.setValue(value, values);
  Form.changeValues([field.key], newValues, form);
};

let testEqual = (expected, value, ()) => ExpectJs.(expect(value) |> toEqual(expected));
let testList = (expected, value) => testEqual(expected |> Belt.List.toArray, value |> Belt.List.toArray);
let testOptionString = (expected, value, ()) =>
  ExpectJs.(expect(value |> Belt.Option.getWithDefault(_, "--None--")) |> toEqual(expected));

let testMany = (describeMessage, list) =>
  describe(describeMessage, () =>
    Belt.List.forEachWithIndex(list, (index, (message, testItem)) =>
      test(message == "" ? describeMessage ++ " #" ++ string_of_int(index) : message) @@ testItem
    )
  );

describe("#user form", () => {
  let form = User.initializeForm();

  testMany(
    "test initial",
    [
      ("form is not dirty", testEqual(false, Form.formIsDirty(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(true, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, User.lastname.bind.hasFocus(form))),
      ("lastname blur", testEqual(true, User.lastname.bind.isBlur(form))),
      ("lastname dirty", testEqual(false, User.lastname.bind.isDirty(form))),
      ("lastname has error", testEqual(true, User.lastname.bind.hasError(form))),
      ("lastname get errors", testList(["Lastname is required."], User.lastname.bind.getErrors(form))),
      ("lastname value", testOptionString("--None--", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, User.firstname.bind.hasFocus(form))),
      ("firstname blur", testEqual(true, User.firstname.bind.isBlur(form))),
      ("firstname dirty", testEqual(false, User.firstname.bind.isDirty(form))),
      ("firstname has error", testEqual(true, User.firstname.bind.hasError(form))),
      ("firstname get errors", testList(["Firstname is required."], User.firstname.bind.getErrors(form))),
      ("firstname value", testOptionString("--None--", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, User.username.bind.hasFocus(form))),
      ("username blur", testEqual(true, User.username.bind.isBlur(form))),
      ("username dirty", testEqual(false, User.username.bind.isDirty(form))),
      ("username has error", testEqual(true, User.username.bind.hasError(form))),
      ("username get errors", testList(["Username is required."], User.username.bind.getErrors(form))),
      ("username value", testOptionString("--None--", Form.getValues(form).username)),
      ("age focus", testEqual(false, User.age.bind.hasFocus(form))),
      ("age blur", testEqual(true, User.age.bind.isBlur(form))),
      ("age dirty", testEqual(false, User.age.bind.isDirty(form))),
      ("age has error", testEqual(false, User.age.bind.hasError(form))),
      ("age get errors", testList([], User.age.bind.getErrors(form))),
      ("age value", testEqual(18, Form.getValues(form).age)),
      ("tags focus", testEqual(false, User.tags.bind.hasFocus(form))),
      ("tags blur", testEqual(true, User.tags.bind.isBlur(form))),
      ("tags dirty", testEqual(false, User.tags.bind.isDirty(form))),
      ("tags has error", testEqual(false, User.tags.bind.hasError(form))),
      ("tags get errors", testList([], User.tags.bind.getErrors(form))),
      ("tags values", testList([], Form.getValues(form).tags)),
      ("tags count", testEqual(0, User.tags.count(Form.getValues(form)))),
    ],
  );

  let form =
    form
    |> User.lastname.bind.focus
    |> changeValue(User.lastname, Some("Maarek"))
    |> User.lastname.bind.blur
    |> User.firstname.bind.focus
    |> changeValue(User.firstname, Some("Joseph"))
    |> User.firstname.bind.blur
    |> User.username.bind.focus
    |> changeValue(User.username, Some("maarekj"))
    |> User.username.bind.blur
    |> User.age.bind.focus
    |> changeValue(User.age, 28)
    |> User.age.bind.blur;

  let form = form |> FieldList.changeValues(User.tags, User.tags.push("tag1", Form.getValues(form)));
  let form = form |> FieldList.changeValues(User.tags, User.tags.push("tag2", Form.getValues(form)));
  let form = form |> FieldList.changeValues(User.tags, User.tags.push("tag3", Form.getValues(form)));
  let form = form |> FieldList.changeValues(User.tags, User.tags.insert(0, "tag0", Form.getValues(form)));
  let form = form |> FieldList.changeValues(User.tags, User.tags.insert(4, "tag4", Form.getValues(form)));

  testMany(
    "test after changes",
    [
      ("form is dirty", testEqual(true, Form.formIsDirty(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(true, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, User.lastname.bind.hasFocus(form))),
      ("lastname blur", testEqual(true, User.lastname.bind.isBlur(form))),
      ("lastname dirty", testEqual(true, User.lastname.bind.isDirty(form))),
      ("lastname has error", testEqual(false, User.lastname.bind.hasError(form))),
      ("lastname get errors", testList([], User.lastname.bind.getErrors(form))),
      ("new value of lastname", testOptionString("Maarek", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, User.firstname.bind.hasFocus(form))),
      ("firstname blur", testEqual(true, User.firstname.bind.isBlur(form))),
      ("firstname dirty", testEqual(true, User.firstname.bind.isDirty(form))),
      ("firstname has error", testEqual(false, User.firstname.bind.hasError(form))),
      ("firstname get errors", testList([], User.firstname.bind.getErrors(form))),
      ("new value of firstname", testOptionString("Joseph", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, User.username.bind.hasFocus(form))),
      ("username blur", testEqual(true, User.username.bind.isBlur(form))),
      ("username dirty", testEqual(true, User.username.bind.isDirty(form))),
      ("username has error", testEqual(false, User.username.bind.hasError(form))),
      ("username get errors", testList([], User.username.bind.getErrors(form))),
      ("new value of username", testOptionString("maarekj", Form.getValues(form).username)),
      ("age focus", testEqual(false, User.age.bind.hasFocus(form))),
      ("age blur", testEqual(true, User.age.bind.isBlur(form))),
      ("age dirty", testEqual(true, User.age.bind.isDirty(form))),
      ("age has error", testEqual(false, User.age.bind.hasError(form))),
      ("age get errors", testList([], User.age.bind.getErrors(form))),
      ("new value of age", testEqual(28, Form.getValues(form).age)),
      ("tags focus", testEqual(false, User.tags.bind.hasFocus(form))),
      ("tags blur", testEqual(true, User.tags.bind.isBlur(form))),
      ("tags dirty", testEqual(true, User.tags.bind.isDirty(form))),
      ("tags has error", testEqual(false, User.tags.bind.hasError(form))),
      ("tags get errors", testList([], User.tags.bind.getErrors(form))),
      ("new value of tags", testList(["tag0", "tag1", "tag2", "tag3", "tag4"], Form.getValues(form).tags)),
      ("tags count", testEqual(5, User.tags.count(Form.getValues(form)))),
    ],
  );
});
