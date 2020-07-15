open Jest;

module Address = Example_address_fields;
module Metadata = Example_metadata_fields;
module User = Example_user_fields;

let changeValue = (field: Field.t('values, 'v), value: 'v, form: Form.form('values, string)) => {
  let values = Form.getValues(form);
  let newValues = field.setValue(value, values);
  Form.changeValues([field.key], newValues, form);
};

let testEqual = (expected, value, ()) => ExpectJs.(expect(value) |> toEqual(expected));
let testTrue = (value, ()) => ExpectJs.(expect(value) |> toBeTruthy);
let testBe = (expected, value, ()) => ExpectJs.(expect(value) |> toBe(expected));
let testList = (expected, value) => testEqual(expected |> Belt.List.toArray, value |> Belt.List.toArray);
let testStringMap = (expected, value) => testEqual(expected |> Belt.List.toArray, value |> Belt.Map.String.toArray);
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
      ("form is not already blur", testEqual(false, Form.formIsAlreadyBlur(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(true, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, Helper.hasFocus(`field(User.lastname), form))),
      ("lastname blur", testEqual(true, Helper.isBlur(`field(User.lastname), form))),
      ("lastname dirty", testEqual(false, Helper.isDirty(`field(User.lastname), form))),
      ("lastname already blur", testEqual(false, Helper.isAlreadyBlur(`field(User.lastname), form))),
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
      ("username already blur", testEqual(false, Helper.isAlreadyBlur(`field(User.username), form))),
      ("username has error", testEqual(true, Helper.hasError(`field(User.username), form))),
      ("username get errors", testList(["Username is required."], Helper.getErrors(`field(User.username), form))),
      ("username value", testOptionString("--None--", Form.getValues(form).username)),
      ("age focus", testEqual(false, Helper.hasFocus(`field(User.age), form))),
      ("age blur", testEqual(true, Helper.isBlur(`field(User.age), form))),
      ("age dirty", testEqual(false, Helper.isDirty(`field(User.age), form))),
      ("age already blur", testEqual(false, Helper.isAlreadyBlur(`field(User.age), form))),
      ("age has error", testEqual(false, Helper.hasError(`field(User.age), form))),
      ("age get errors", testList([], Helper.getErrors(`field(User.age), form))),
      ("age value", testEqual(18, Form.getValues(form).age)),
      ("tags focus", testEqual(false, Helper.hasFocus(`list(User.tags), form))),
      ("tags blur", testEqual(true, Helper.isBlur(`list(User.tags), form))),
      ("tags dirty", testEqual(false, Helper.isDirty(`list(User.tags), form))),
      ("tags already blur", testEqual(false, Helper.isAlreadyBlur(`list(User.tags), form))),
      ("tags has error", testEqual(false, Helper.hasError(`list(User.tags), form))),
      ("tags get errors", testList([], Helper.getErrors(`list(User.tags), form))),
      ("tags values", testList([], Form.getValues(form).tags)),
      ("tags count", testEqual(0, User.tags.count(Form.getValues(form)))),
      ("addresses focus", testEqual(false, Helper.hasFocus(`list(User.addresses), form))),
      ("addresses blur", testEqual(true, Helper.isBlur(`list(User.addresses), form))),
      ("addresses dirty", testEqual(false, Helper.isDirty(`list(User.addresses), form))),
      ("addresses already blur", testEqual(false, Helper.isAlreadyBlur(`list(User.addresses), form))),
      ("addresses count", testEqual(0, User.addresses.count(Form.getValues(form)))),
      ("addresses values", testList([], Form.getValues(form).addresses)),
      (
        "addresses get errors",
        testList(["Must contains one address at least."], Helper.getErrors(`list(User.addresses), form)),
      ),
      ("metadata focus", testEqual(false, Helper.hasFocus(`map(User.metadata), form))),
      ("metadata blur", testEqual(true, Helper.isBlur(`map(User.metadata), form))),
      ("metadata dirty", testEqual(false, Helper.isDirty(`map(User.metadata), form))),
      ("metadata already blur", testEqual(false, Helper.isAlreadyBlur(`map(User.metadata), form))),
      ("metadata count keys", testEqual(2, User.metadata.countKeys(Form.getValues(form)))),
      ("metadata all keys", testList(["option1", "option2"], User.metadata.allKeys(Form.getValues(form)))),
      (
        "metadata values",
        testStringMap(
          [("option1", Metadata.Value.empty), ("option2", Metadata.Value.empty)],
          Form.getValues(form).metadata,
        ),
      ),
      ("metadata get errors", testList([], Helper.getErrors(`map(User.metadata), form))),
      (
        "metadata.option1.title get errors",
        testList(
          ["Title is required."],
          Helper.getErrors(`field(User.metadata.getFields("option1").title), form),
        ),
      ),
      (
        "metadata.option1.desc get errors",
        testList([], Helper.getErrors(`field(User.metadata.getFields("option1").desc), form)),
      ),
      (
        "metadata.option2.title get errors",
        testList(
          ["Title is required."],
          Helper.getErrors(`field(User.metadata.getFields("option2").title), form),
        ),
      ),
      (
        "metadata.option2.desc get errors",
        testList([], Helper.getErrors(`field(User.metadata.getFields("option2").desc), form)),
      ),
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

  let form =
    form
    |> Form.changeValues(
         [User.addresses.key],
         User.addresses.push({Address.Value.city: "Choisy le roi", street: "6 rue bascou"}, Form.getValues(form)),
       );

  let form =
    form
    |> changeValue(User.metadata.getFields("option1").title, "Option1 Title")
    |> changeValue(User.metadata.getFields("option1").desc, "Option1 Description")
    |> changeValue(User.metadata.getFields("option2").title, "Option2 Title");

  testMany(
    "test after changes",
    [
      ("form is dirty", testEqual(true, Form.formIsDirty(form))),
      ("form is already blur", testEqual(true, Form.formIsAlreadyBlur(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(false, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, Helper.hasFocus(`field(User.lastname), form))),
      ("lastname blur", testEqual(true, Helper.isBlur(`field(User.lastname), form))),
      ("lastname dirty", testEqual(true, Helper.isDirty(`field(User.lastname), form))),
      ("lastname already blur", testEqual(true, Helper.isAlreadyBlur(`field(User.lastname), form))),
      ("lastname has error", testEqual(false, Helper.hasError(`field(User.lastname), form))),
      ("lastname get errors", testList([], Helper.getErrors(`field(User.lastname), form))),
      ("new value of lastname", testOptionString("Maarek", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, Helper.hasFocus(`field(User.firstname), form))),
      ("firstname blur", testEqual(true, Helper.isBlur(`field(User.firstname), form))),
      ("firstname dirty", testEqual(true, Helper.isDirty(`field(User.firstname), form))),
      ("firstname already blur", testEqual(true, Helper.isAlreadyBlur(`field(User.firstname), form))),
      ("firstname has error", testEqual(false, Helper.hasError(`field(User.firstname), form))),
      ("firstname get errors", testList([], Helper.getErrors(`field(User.firstname), form))),
      ("new value of firstname", testOptionString("Joseph", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, Helper.hasFocus(`field(User.username), form))),
      ("username blur", testEqual(true, Helper.isBlur(`field(User.username), form))),
      ("username dirty", testEqual(true, Helper.isDirty(`field(User.username), form))),
      ("username already blur", testEqual(true, Helper.isAlreadyBlur(`field(User.username), form))),
      ("username has error", testEqual(false, Helper.hasError(`field(User.username), form))),
      ("username get errors", testList([], Helper.getErrors(`field(User.username), form))),
      ("new value of username", testOptionString("maarekj", Form.getValues(form).username)),
      ("age focus", testEqual(false, Helper.hasFocus(`field(User.age), form))),
      ("age blur", testEqual(true, Helper.isBlur(`field(User.age), form))),
      ("age dirty", testEqual(true, Helper.isDirty(`field(User.age), form))),
      ("age already blur", testEqual(true, Helper.isAlreadyBlur(`field(User.age), form))),
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
