/*open Jest;

module User = {
  type t = {
    username: option(string),
    lastname: option(string),
    firstname: option(string),
    age: int,
  };
  let initialUser = {
    username: None,
    lastname: None,
    firstname: None,
    age: 18,
  };
  let lastname =
    Field.createField(~key="lastname", ~changeValue=(value, values) =>
      {...values, lastname: value}
    );
  let firstname =
    Field.createField(~key="firstname", ~changeValue=(value, values) =>
      {...values, firstname: value}
    );
  let username =
    Field.createField(~key="username", ~changeValue=(value, values) =>
      {...values, username: value}
    );
  let age =
    Field.createField(~key="age", ~changeValue=(value, values) =>
      {...values, age: value}
    );
  let onValidate = form => {
    let id = form => form;
    let values = Form.getValues(form);
    form
    |> (
      switch (values.lastname) {
      | None
      | Some("") => lastname.addError("Lastname is required")
      | _ => id
      }
    )
    |> (
      switch (values.firstname) {
      | None
      | Some("") => firstname.addError("Firstname is required")
      | _ => id
      }
    )
    |> (
      switch (values.username) {
      | None
      | Some("") => username.addError("Username is required")
      | _ => id
      }
    )
    |> (
      switch (values.age) {
      | a when a < 18 => age.addError("You must be major")
      | _ => id
      }
    );
  };
  let initializeForm = () =>
    Form.initializeForm(~initialValues=initialUser, ~onValidate, ());
};

describe("#user form", () => {
  open ExpectJs;
  let form = User.initializeForm();
  test("focus field") @@
  (() => expect(User.lastname.hasFocus(form)) |> toEqual(false));
  let form = User.lastname.focus(form);
  test("focus field") @@
  (() => expect(User.lastname.hasFocus(form)) |> toEqual(true));
  test("get values") @@
  (
    () =>
      expect(
        Form.getValues(form).lastname |> Belt.Option.getWithDefault(_, ""),
      )
      |> toEqual("")
  );
  let form = User.lastname.changeValue(Some("Maarek"), form);
  test("change value") @@
  (
    () =>
      expect(
        Form.getValues(form).lastname |> Belt.Option.getWithDefault(_, ""),
      )
      |> toEqual("Maarek")
  );
  test("get errors") @@
  (
    () =>
      expect(User.lastname.getErrors(form) |> Belt.List.toArray)
      |> toEqual([||])
  );

  let form = User.lastname.changeValue(Some(""), form);
  test("change value") @@
  (
    () =>
      expect(
        Form.getValues(form).lastname |> Belt.Option.getWithDefault(_, ""),
      )
      |> toEqual("")
  );
  test("get errors") @@
  (
    () =>
      expect(User.lastname.getErrors(form) |> Belt.List.toArray)
      |> toEqual([|"Lastname is required"|])
  );
});*/