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

  type form = Form.form(t, string);

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
        ({wrapField}) =>
          {numero: wrapField(Address.numero), street: wrapField(Address.street), city: wrapField(Address.city)},
    );

  let mainAddress =
    FieldObject.createField(
      ~key="mainAddress",
      ~getObject=values => values.mainAddress,
      ~setObject=(value, values) => {...values, mainAddress: value},
      ~createFields=
        ({wrapField}) =>
          {
            numero: wrapField(Field.wrapOptionField(~field=Address.numero, ~empty=Address.empty, ())),
            street: wrapField(Field.wrapOptionField(~field=Address.street, ~empty=Address.empty, ())),
            city: wrapField(Field.wrapOptionField(~field=Address.city, ~empty=Address.empty, ())),
          },
    );

  let onValidate = form => {
    let id = form => form;
    let values = Form.getValues(form);
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
      Belt.List.size(values.addresses) < 2
        ? addError(`list(addresses), "Must contains one address at least.", form) : form;

    let validateAddress = (address: Address.t, field: address, form) =>
      form
      |> (
        switch (address.street) {
        | "" => addError(`field(field.street), "Street is required.")
        | "forbidden" => addError(`field(field.street), "Forbidden street.")
        | _ => id
        }
      )
      |> (
        switch (address.city) {
        | "" => addError(`field(field.city), "City is required.")
        | "forbidden" => addError(`field(field.city), "Forbidden city.")
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
             | "" => addError(`field(tags.getRow(i)), "Tag is required.")
             | "forbidden" => addError(`field(tags.getRow(i)), "Forbidden tag.")
             | _ => id
             }
           )
         );
    form;
  };
  let initializeForm = () => Form.initializeForm(~initialValues=initialUser, ~onValidate, ());
};

let changeValue = (field: Field.t('values, 'v), value: 'v, form: Form.form('values, string)) => {
  let values = Form.getValues(form);
  let newValues = field.setValue(value, values);
  Form.changeValues([field.key], newValues, form);
};

let testEqual = (expected, value, ()) => ExpectJs.(expect(value) |> toEqual(expected));
let testTrue = (value, ()) => ExpectJs.(expect(value) |> toBeTruthy);
let testBe = (expected, value, ()) => ExpectJs.(expect(value) |> toBe(expected));
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
      ("lastname focus", testEqual(false, Helper.hasFocus(`field(User.lastname), form))),
      ("lastname blur", testEqual(true, Helper.isBlur(`field(User.lastname), form))),
      ("lastname dirty", testEqual(false, Helper.isDirty(`field(User.lastname), form))),
      ("lastname has error", testEqual(true, Helper.hasError(`field(User.lastname), form))),
      ("lastname get errors", testList(["Lastname is required."], Helper.getErrors(`field(User.lastname), form))),
      ("lastname value", testOptionString("--None--", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, Helper.hasFocus(`field(User.firstname), form))),
      ("firstname blur", testEqual(true, Helper.isBlur(`field(User.firstname), form))),
      ("firstname dirty", testEqual(false, Helper.isDirty(`field(User.firstname), form))),
      ("firstname has error", testEqual(true, Helper.hasError(`field(User.firstname), form))),
      (
        "firstname get errors",
        testList(["Firstname is required."], Helper.getErrors(`field(User.firstname), form)),
      ),
      ("firstname value", testOptionString("--None--", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, Helper.hasFocus(`field(User.username), form))),
      ("username blur", testEqual(true, Helper.isBlur(`field(User.username), form))),
      ("username dirty", testEqual(false, Helper.isDirty(`field(User.username), form))),
      ("username has error", testEqual(true, Helper.hasError(`field(User.username), form))),
      ("username get errors", testList(["Username is required."], Helper.getErrors(`field(User.username), form))),
      ("username value", testOptionString("--None--", Form.getValues(form).username)),
      ("age focus", testEqual(false, Helper.hasFocus(`field(User.age), form))),
      ("age blur", testEqual(true, Helper.isBlur(`field(User.age), form))),
      ("age dirty", testEqual(false, Helper.isDirty(`field(User.age), form))),
      ("age has error", testEqual(false, Helper.hasError(`field(User.age), form))),
      ("age get errors", testList([], Helper.getErrors(`field(User.age), form))),
      ("age value", testEqual(18, Form.getValues(form).age)),
      ("tags focus", testEqual(false, Helper.hasFocus(`list(User.tags), form))),
      ("tags blur", testEqual(true, Helper.isBlur(`list(User.tags), form))),
      ("tags dirty", testEqual(false, Helper.isDirty(`list(User.tags), form))),
      ("tags has error", testEqual(false, Helper.hasError(`list(User.tags), form))),
      ("tags get errors", testList([], Helper.getErrors(`list(User.tags), form))),
      ("tags values", testList([], Form.getValues(form).tags)),
      ("tags count", testEqual(0, User.tags.count(Form.getValues(form)))),
    ],
  );

  let form =
    form
    |> Helper.focus(`field(User.lastname))
    |> changeValue(User.lastname, Some("Maarek"))
    |> Helper.blur(`field(User.lastname))
    |> Helper.focus(`field(User.firstname))
    |> changeValue(User.firstname, Some("Joseph"))
    |> Helper.blur(`field(User.firstname))
    |> Helper.focus(`field(User.username))
    |> changeValue(User.username, Some("maarekj"))
    |> Helper.blur(`field(User.username))
    |> Helper.focus(`field(User.age))
    |> changeValue(User.age, 28)
    |> Helper.blur(`field(User.age));

  let form = form |> Form.changeValues([User.tags.key], User.tags.push("tag1", Form.getValues(form)));
  let form = form |> Form.changeValues([User.tags.key], User.tags.push("tag2", Form.getValues(form)));
  let form = form |> Form.changeValues([User.tags.key], User.tags.push("tag3", Form.getValues(form)));
  let form = form |> Form.changeValues([User.tags.key], User.tags.insert(0, "tag0", Form.getValues(form)));
  let form = form |> Form.changeValues([User.tags.key], User.tags.insert(4, "tag4", Form.getValues(form)));

  testMany(
    "test after changes",
    [
      ("form is dirty", testEqual(true, Form.formIsDirty(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(true, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, Helper.hasFocus(`field(User.lastname), form))),
      ("lastname blur", testEqual(true, Helper.isBlur(`field(User.lastname), form))),
      ("lastname dirty", testEqual(true, Helper.isDirty(`field(User.lastname), form))),
      ("lastname has error", testEqual(false, Helper.hasError(`field(User.lastname), form))),
      ("lastname get errors", testList([], Helper.getErrors(`field(User.lastname), form))),
      ("new value of lastname", testOptionString("Maarek", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, Helper.hasFocus(`field(User.firstname), form))),
      ("firstname blur", testEqual(true, Helper.isBlur(`field(User.firstname), form))),
      ("firstname dirty", testEqual(true, Helper.isDirty(`field(User.firstname), form))),
      ("firstname has error", testEqual(false, Helper.hasError(`field(User.firstname), form))),
      ("firstname get errors", testList([], Helper.getErrors(`field(User.firstname), form))),
      ("new value of firstname", testOptionString("Joseph", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, Helper.hasFocus(`field(User.username), form))),
      ("username blur", testEqual(true, Helper.isBlur(`field(User.username), form))),
      ("username dirty", testEqual(true, Helper.isDirty(`field(User.username), form))),
      ("username has error", testEqual(false, Helper.hasError(`field(User.username), form))),
      ("username get errors", testList([], Helper.getErrors(`field(User.username), form))),
      ("new value of username", testOptionString("maarekj", Form.getValues(form).username)),
      ("age focus", testEqual(false, Helper.hasFocus(`field(User.age), form))),
      ("age blur", testEqual(true, Helper.isBlur(`field(User.age), form))),
      ("age dirty", testEqual(true, Helper.isDirty(`field(User.age), form))),
      ("age has error", testEqual(false, Helper.hasError(`field(User.age), form))),
      ("age get errors", testList([], Helper.getErrors(`field(User.age), form))),
      ("new value of age", testEqual(28, Form.getValues(form).age)),
      ("tags focus", testEqual(false, Helper.hasFocus(`list(User.tags), form))),
      ("tags blur", testEqual(true, Helper.isBlur(`list(User.tags), form))),
      ("tags dirty", testEqual(true, Helper.isDirty(`list(User.tags), form))),
      ("tags has error", testEqual(false, Helper.hasError(`list(User.tags), form))),
      ("tags get errors", testList([], Helper.getErrors(`list(User.tags), form))),
      ("new value of tags", testList(["tag0", "tag1", "tag2", "tag3", "tag4"], Form.getValues(form).tags)),
      ("tags count", testEqual(5, User.tags.count(Form.getValues(form)))),
    ],
  );

  let form = User.initializeForm();
  let form1 =
    form
    |> Helper.focus(`field(User.lastname))
    |> changeValue(User.lastname, Some("Maarek"))
    |> Helper.blur(`field(User.lastname));
  let form2 =
    form1
    |> Helper.focus(`field(User.lastname))
    |> changeValue(User.lastname, Some("Maarek"))
    |> Helper.blur(`field(User.lastname));
  let form3 =
    form2
    |> Helper.focus(`field(User.lastname))
    |> changeValue(User.lastname, Some("Maarek"))
    |> Helper.blur(`field(User.lastname));
  testMany(
    "test physic equal is respected",
    [
      (
        "meta field after change value",
        testTrue(
          Form.Eq.metaField(Form.getField(User.lastname.key, form2), Form.getField(User.lastname.key, form3)),
        ),
      ),
    ],
  );
});
