open Jest;
module Address = Example_address_fields;
module Metadata = Example_metadata_fields;
module User = Example_user_fields;

let changeValue = Helper.changeFieldValue;
let hasError = f => Form.hasError(f.Field.key);
let hasFocus = f => Form.hasFocus(f.Field.key);
let isBlur = f => Form.isBlur(f.Field.key);
let isDirty = f => Form.isDirty(f.Field.key);
let isAlreadyBlur = f => Form.isAlreadyBlur(f.Field.key);
let getErrors = f => Form.getErrors(f.Field.key);

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
  let fields = User.fields;
  let form = User.initializeForm(User.Value.empty);
  testMany(
    "test initial",
    [
      ("form is not dirty", testEqual(false, Form.formIsDirty(form))),
      ("form is not already blur", testEqual(false, Form.formIsAlreadyBlur(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(true, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, hasFocus(fields.lastname, form))),
      ("lastname blur", testEqual(true, isBlur(fields.lastname, form))),
      ("lastname dirty", testEqual(false, isDirty(fields.lastname, form))),
      ("lastname already blur", testEqual(false, isAlreadyBlur(fields.lastname, form))),
      ("lastname has error", testEqual(true, hasError(fields.lastname, form))),
      ("lastname get errors", testList(["Lastname is required."], getErrors(fields.lastname, form))),
      ("lastname value", testOptionString("--None--", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, hasFocus(fields.firstname, form))),
      ("firstname blur", testEqual(true, isBlur(fields.firstname, form))),
      ("firstname dirty", testEqual(false, isDirty(fields.firstname, form))),
      ("firstname has error", testEqual(true, hasError(fields.firstname, form))),
      ("firstname get errors", testList(["Firstname is required."], getErrors(fields.firstname, form))),
      ("firstname value", testOptionString("--None--", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, hasFocus(fields.username, form))),
      ("username blur", testEqual(true, isBlur(fields.username, form))),
      ("username dirty", testEqual(false, isDirty(fields.username, form))),
      ("username already blur", testEqual(false, isAlreadyBlur(fields.username, form))),
      ("username has error", testEqual(true, hasError(fields.username, form))),
      ("username get errors", testList(["Username is required."], getErrors(fields.username, form))),
      ("username value", testOptionString("--None--", Form.getValues(form).username)),
      ("age focus", testEqual(false, hasFocus(fields.age, form))),
      ("age blur", testEqual(true, isBlur(fields.age, form))),
      ("age dirty", testEqual(false, isDirty(fields.age, form))),
      ("age already blur", testEqual(false, isAlreadyBlur(fields.age, form))),
      ("age has error", testEqual(false, hasError(fields.age, form))),
      ("age get errors", testList([], getErrors(fields.age, form))),
      ("age value", testEqual(18, Form.getValues(form).age)),
      ("tags focus", testEqual(false, hasFocus(fields.tags->fst, form))),
      ("tags blur", testEqual(true, isBlur(fields.tags->fst, form))),
      ("tags dirty", testEqual(false, isDirty(fields.tags->fst, form))),
      ("tags already blur", testEqual(false, isAlreadyBlur(fields.tags->fst, form))),
      ("tags has error", testEqual(false, hasError(fields.tags->fst, form))),
      ("tags get errors", testList([], getErrors(fields.tags->fst, form))),
      ("tags values", testList([], Form.getValues(form).tags)),
      ("tags count", testEqual(0, fields.tags->fst.getValue(Form.getValues(form))->Belt.List.length)),
      ("addresses focus", testEqual(false, hasFocus(fields.addresses->fst, form))),
      ("addresses blur", testEqual(true, isBlur(fields.addresses->fst, form))),
      ("addresses dirty", testEqual(false, isDirty(fields.addresses->fst, form))),
      ("addresses already blur", testEqual(false, isAlreadyBlur(fields.addresses->fst, form))),
      ("addresses count", testEqual(0, fields.addresses->fst.getValue(Form.getValues(form))->Belt.List.length)),
      ("addresses values", testList([], Form.getValues(form).addresses)),
      (
        "addresses get errors",
        testList(["Must contains one address at least."], getErrors(fields.addresses->fst, form)),
      ),
      ("metadata focus", testEqual(false, hasFocus(fields.metadata->fst, form))),
      ("metadata blur", testEqual(true, isBlur(fields.metadata->fst, form))),
      ("metadata dirty", testEqual(false, isDirty(fields.metadata->fst, form))),
      ("metadata already blur", testEqual(false, isAlreadyBlur(fields.metadata->fst, form))),
      (
        "metadata count keys",
        testEqual(
          2,
          fields.metadata->fst.getValue(Form.getValues(form))->Belt.Map.String.keysToArray->Belt.Array.length,
        ),
      ),
      (
        "metadata all keys",
        testList(
          ["option1", "option2"],
          fields.metadata->fst.getValue(Form.getValues(form))->Belt.Map.String.keysToArray->Belt.List.fromArray,
        ),
      ),
      (
        "metadata values",
        testStringMap(
          [("option1", Metadata.Value.empty), ("option2", Metadata.Value.empty)],
          Form.getValues(form).metadata,
        ),
      ),
      ("metadata get errors", testList([], getErrors(fields.metadata->fst, form))),
      (
        "metadata.option1.title get errors",
        testList(["Title is required."], getErrors(fields.metadata->snd("option1").title, form)),
      ),
      ("metadata.option1.desc get errors", testList([], getErrors(fields.metadata->snd("option1").desc, form))),
      (
        "metadata.option2.title get errors",
        testList(["Title is required."], getErrors(fields.metadata->snd("option2").title, form)),
      ),
      ("metadata.option2.desc get errors", testList([], getErrors(fields.metadata->snd("option2").desc, form))),
    ],
  );

  let form =
    form
    |> Form.focus(fields.lastname.key)
    |> changeValue(fields.lastname, Some("Maarek"))
    |> Form.blur(fields.lastname.key)
    |> Form.focus(fields.firstname.key)
    |> changeValue(fields.firstname, Some("Joseph"))
    |> Form.blur(fields.firstname.key)
    |> Form.focus(fields.username.key)
    |> changeValue(fields.username, Some("maarekj"))
    |> Form.blur(fields.username.key)
    |> Form.focus(fields.age.key)
    |> changeValue(fields.age, 28)
    |> Form.blur(fields.age.key)
    |> Helper.List.push(fields.tags->fst, "tag1")
    |> Helper.List.push(fields.tags->fst, "tag2")
    |> Helper.List.push(fields.tags->fst, "tag3")
    |> Helper.List.insert(fields.tags->fst, 0, "tag0")
    |> Helper.List.insert(fields.tags->fst, 4, "tag4")
    |> Helper.List.push(
         fields.addresses->fst,
         {Address.Value.city: "Choisy le roi", street: "6 rue bascou", zipcode: Some("94600")},
       )
    |> changeValue(fields.metadata->snd("option1").title, "Option1 Title")
    |> changeValue(fields.metadata->snd("option1").desc, "Option1 Description")
    |> changeValue(fields.metadata->snd("option2").title, "Option2 Title");

  testMany(
    "test after changes",
    [
      ("form is dirty", testEqual(true, Form.formIsDirty(form))),
      ("form is already blur", testEqual(true, Form.formIsAlreadyBlur(form))),
      ("form has not root errors", testEqual(false, Form.formHasRootErrors(form))),
      ("form has errors", testEqual(false, Form.formHasErrors(form))),
      ("form has not submit errors", testEqual(false, Form.formHasSubmitErrors(form))),
      ("lastname focus", testEqual(false, hasFocus(fields.lastname, form))),
      ("lastname blur", testEqual(true, isBlur(fields.lastname, form))),
      ("lastname dirty", testEqual(true, isDirty(fields.lastname, form))),
      ("lastname already blur", testEqual(true, isAlreadyBlur(fields.lastname, form))),
      ("lastname has error", testEqual(false, hasError(fields.lastname, form))),
      ("lastname get errors", testList([], getErrors(fields.lastname, form))),
      ("new value of lastname", testOptionString("Maarek", Form.getValues(form).lastname)),
      ("firstname focus", testEqual(false, hasFocus(fields.firstname, form))),
      ("firstname blur", testEqual(true, isBlur(fields.firstname, form))),
      ("firstname dirty", testEqual(true, isDirty(fields.firstname, form))),
      ("firstname already blur", testEqual(true, isAlreadyBlur(fields.firstname, form))),
      ("firstname has error", testEqual(false, hasError(fields.firstname, form))),
      ("firstname get errors", testList([], getErrors(fields.firstname, form))),
      ("new value of firstname", testOptionString("Joseph", Form.getValues(form).firstname)),
      ("username focus", testEqual(false, hasFocus(fields.username, form))),
      ("username blur", testEqual(true, isBlur(fields.username, form))),
      ("username dirty", testEqual(true, isDirty(fields.username, form))),
      ("username already blur", testEqual(true, isAlreadyBlur(fields.username, form))),
      ("username has error", testEqual(false, hasError(fields.username, form))),
      ("username get errors", testList([], getErrors(fields.username, form))),
      ("new value of username", testOptionString("maarekj", Form.getValues(form).username)),
      ("age focus", testEqual(false, hasFocus(fields.age, form))),
      ("age blur", testEqual(true, isBlur(fields.age, form))),
      ("age dirty", testEqual(true, isDirty(fields.age, form))),
      ("age already blur", testEqual(true, isAlreadyBlur(fields.age, form))),
      ("age has error", testEqual(false, hasError(fields.age, form))),
      ("age get errors", testList([], getErrors(fields.age, form))),
      ("new value of age", testEqual(28, Form.getValues(form).age)),
      ("tags focus", testEqual(false, hasFocus(fields.tags->fst, form))),
      ("tags blur", testEqual(true, isBlur(fields.tags->fst, form))),
      ("tags dirty", testEqual(true, isDirty(fields.tags->fst, form))),
      ("tags has error", testEqual(false, hasError(fields.tags->fst, form))),
      ("tags get errors", testList([], getErrors(fields.tags->fst, form))),
      ("new value of tags", testList(["tag0", "tag1", "tag2", "tag3", "tag4"], Form.getValues(form).tags)),
      ("tags count", testEqual(5, fields.tags->fst.getValue(Form.getValues(form))->Belt.List.length)),
    ],
  );

  let fields = User.fields;
  let form = User.initializeForm(User.Value.empty);

  let form1 =
    form
    |> Form.focus(fields.lastname.key)
    |> changeValue(fields.lastname, Some("Maarek"))
    |> Form.blur(fields.lastname.key);
  let form2 =
    form1
    |> Form.focus(fields.lastname.key)
    |> changeValue(fields.lastname, Some("Maarek"))
    |> Form.blur(fields.lastname.key);
  let form3 =
    form2
    |> Form.focus(fields.lastname.key)
    |> changeValue(fields.lastname, Some("Maarek"))
    |> Form.blur(fields.lastname.key);

  testMany(
    "test physic equal is respected",
    [
      (
        "meta field after change value",
        testTrue(
          Form.Eq.metaField(Form.getField(fields.lastname.key, form2), Form.getField(fields.lastname.key, form3)),
        ),
      ),
    ],
  );
});
