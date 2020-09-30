module Address = Example_address_fields;
module Metadata = Example_metadata_fields;

module Gender = {
  type t =
    | Male
    | Female
    | Other;

  let choices = [
    {BootstrapRender.Choice.value: Male, string: "male", label: "Male"},
    {value: Female, string: "female", label: "Female"},
    {value: Other, string: "other", label: "Other"},
  ];

  let optChoices = [
    {BootstrapRender.Choice.value: None, string: "", label: ""},
    ...choices->Belt.List.map(choice => {...choice, value: Some(choice.value)}),
  ];
};

module Value = {
  type t = {
    username: option(string),
    lastname: option(string),
    firstname: option(string),
    gender: option(Gender.t),
    age: int,
    tags: list(string),
    mainAddress: option(Address.Value.t),
    addresses: list(Address.Value.t),
    metadata: Belt.Map.String.t(Metadata.Value.t),
  };

  let empty = {
    username: None,
    lastname: None,
    firstname: None,
    gender: None,
    age: 18,
    tags: [],
    mainAddress: None,
    addresses: [],
    metadata: Belt.Map.String.fromArray([|("option1", Metadata.Value.empty), ("option2", Metadata.Value.empty)|]),
  };
};

type fields('t, 'self) = {
  self: Field.t('t, 'self),
  username: Field.t('t, option(string)),
  lastname: Field.t('t, option(string)),
  firstname: Field.t('t, option(string)),
  gender: Field.t('t, option(Gender.t)),
  age: Field.t('t, int),
  tags: (Field.t('t, list(string)), int => Field.t('t, string)),
  mainAddress: Address.fields('t, option(Address.Value.t)),
  addresses: (Field.t('t, list(Address.Value.t)), int => Address.fields('t, Address.Value.t)),
  metadata: (Field.t('t, Belt.Map.String.t(Metadata.Value.t)), string => Metadata.fields('t, Metadata.Value.t)),
};
type t('t, 'self) = fields('t, 'self);

let createFields = (self, baseField) =>
  Field.{
    self,
    username:
      baseField
      +|> Field.createField(
            ~key="username",
            ~getValue=v => v.Value.username,
            ~setValue=(username, v) => {...v, username},
          ),
    lastname:
      baseField
      +|> Field.createField(
            ~key="lastname",
            ~getValue=v => v.Value.lastname,
            ~setValue=(lastname, v) => {...v, lastname},
          ),
    firstname:
      baseField
      +|> Field.createField(
            ~key="firstname",
            ~getValue=v => v.Value.firstname,
            ~setValue=(firstname, v) => {...v, firstname},
          ),
    gender:
      baseField
      +|> Field.createField(~key="gender", ~getValue=v => v.Value.gender, ~setValue=(gender, v) => {...v, gender}),
    age: baseField +|> Field.createField(~key="age", ~getValue=v => v.Value.age, ~setValue=(age, v) => {...v, age}),
    tags: {
      let field =
        baseField
        +|> Field.createField(~key="tags", ~getValue=v => v.Value.tags, ~setValue=(tags, v) => {...v, tags});
      (field, makeListItemField("", field));
    },
    mainAddress: {
      let field =
        baseField
        +|> createField(
              ~key="mainAddress",
              ~getValue=v => v.Value.mainAddress,
              ~setValue=(mainAddress, v) => {...v, mainAddress},
            );

      Address.createFields(field, field +?|> Address.Value.empty);
    },
    addresses:
      (
        baseField
        +|> createField(
              ~key="addresses",
              ~getValue=v => v.Value.addresses,
              ~setValue=(addresses, v) => {...v, addresses},
            )
      )
      ->chainList(Address.Value.empty, Address.createFields),
    metadata: {
      (
        baseField
        +|> createField(
              ~key="metadata",
              ~getValue=v => v.Value.metadata,
              ~setValue=(metadata, v) => {...v, metadata},
            )
      )
      ->chainStringMap(Metadata.Value.empty, Metadata.createFields);
    },
  };

let rootField = Field.idField("root");
let fields = createFields(rootField, rootField);

let onValidate = (fields: fields(_, Value.t), form) => {
  open Belt;

  let id = form => form;
  let values: Value.t = Form.getValues(form);
  let addError = f => Form.addError(f.Field.key);

  let form =
    form
    |> (
      switch (values.lastname) {
      | None
      | Some("") => addError(fields.lastname, "Lastname is required.")
      | _ => id
      }
    )
    |> (
      switch (values.firstname) {
      | None
      | Some("") => addError(fields.firstname, "Firstname is required.")
      | _ => id
      }
    )
    |> (
      switch (values.username) {
      | None
      | Some("") => addError(fields.username, "Username is required.")
      | Some("maarek") => addError(fields.username, "Username is already used.")
      | _ => id
      }
    )
    |> (
      switch (values.age) {
      | a when a < 18 => addError(fields.age, "You must be major.")
      | _ => id
      }
    );

  let form =
    List.size(values.addresses) < 1
      ? addError(fields.addresses->fst, "Must contains one address at least.", form) : form;

  let form = form |> Address.validateOptional(fields.mainAddress);

  let form =
    List.mapWithIndex(values.addresses, (i, a) => (i, a))
    |> List.reduce(_, form, (form, (i, _)) => form |> Address.validate(fields.addresses->snd(i)));

  let form =
    List.mapWithIndex(values.tags, (i, a) => (i, a))
    |> List.reduce(_, form, (form, (i, tag)) =>
         form
         |> (
           switch (tag) {
           | "" => addError(fields.tags->snd(i), "Tag is required.")
           | "forbidden" => addError(fields.tags->snd(i), "Forbidden tag.")
           | _ => id
           }
         )
       );

  let form =
    values.metadata
    ->Belt.Map.String.toList
    ->Belt.List.reduce(form, (form, (key, _)) => {form |> Metadata.validate(fields.metadata->snd(key))});

  form;
};

let initializeForm = initialValues =>
  ReasonForm.Form.initializeForm(~initialValues, ~onValidate=onValidate(fields), ());

let useForm = initialValues => {
  React.useMemo0(() => {
    let form = initializeForm(initialValues);

    (ReasonForm.Wrap.make(form), fields);
  });
};
