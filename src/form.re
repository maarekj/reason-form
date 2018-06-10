module SMap = Belt.Map.String;

type metaField = {
  focus: bool,
  dirty: bool,
  errors: list(string),
};

type metaFields = SMap.t(metaField);

let emptyField = {focus: false, dirty: false, errors: []};

type form('a) = {
  fields: metaFields,
  globalErrors: list(string),
  onValidate: form('a) => form('a),
  onBlur: (string, form('a)) => form('a),
  onFocus: (string, form('a)) => form('a),
  onChangeValue: form('a) => form('a),
  initialValues: 'a,
  values: 'a,
};

let initializeForm =
    (
      ~initialValues,
      ~globalErrors=[],
      ~onBlur=(_key, form) => form,
      ~onFocus=(_key, form) => form,
      ~onChangeValue=form => form,
      ~onValidate=form => form,
      (),
    ) => {
  let form = {
    fields: SMap.empty,
    globalErrors,
    onValidate,
    onBlur,
    onFocus,
    onChangeValue,
    initialValues,
    values: initialValues,
  };
  form |> onValidate;
};

let mapFields = (form, f) => {...form, fields: f(form.fields)};

let mapField = (form, f, key) =>
  mapFields(
    form,
    fields => {
      let field =
        SMap.get(fields, key) |> Belt.Option.getWithDefault(_, emptyField);
      SMap.set(fields, key, f(field));
    },
  );

let getField = (key, form) =>
  SMap.get(form.fields, key) |> Belt.Option.getWithDefault(_, emptyField);

let focus = (key, form) =>
  mapField(form, field => {...field, focus: true}, key) |> form.onFocus(key);

let blur = (key, form) =>
  mapField(form, field => {...field, focus: false, dirty: true}, key)
  |> form.onBlur(key);

let addGlobalError = (error, form) => {
  ...form,
  globalErrors: [error, ...form.globalErrors],
};

let addError = (key, error, form) =>
  mapField(
    form,
    field => {...field, errors: [error, ...field.errors]},
    key,
  );

let clearErrors = (key, form) =>
  mapField(form, field => {...field, errors: []}, key);

let clearAllFieldsErrors = form =>
  mapFields(
    form,
    fields => {
      let keys = SMap.keysToArray(fields);
      Belt.Array.reduce(
        keys,
        fields,
        (fields, key) => {
          let field = SMap.get(fields, key);
          field
          |> Belt.Option.map(_, field =>
               SMap.set(fields, key, {...field, errors: []})
             )
          |> Belt.Option.getWithDefault(_, fields);
        },
      );
    },
  );

let changeValues = (keys, values, form) => {
  let form = {...form, values};
  List.fold_left(
    (form, key) => mapField(form, field => {...field, dirty: true}, key),
    form,
    keys,
  )
  |> form.onChangeValue
  |> clearAllFieldsErrors
  |> form.onValidate;
};

let hasFocus = (key, form) => getField(key, form).focus;

let isBlur = (key, form) => ! getField(key, form).focus;

let isDirty = (key, form) => getField(key, form).dirty;

let hasError = (key, form) => List.length(getField(key, form).errors) > 0;

let getErrors = (key, form) => getField(key, form).errors;

let getValues = form => form.values;

let getInitialValues = form => form.initialValues;

let formIsDirty = form =>
  SMap.reduce(form.fields, false, (acc, _key, field) => acc || field.dirty);

let formHasGlobalError = form => List.length(form.globalErrors) > 0;

let getGlobalErrors = form => form.globalErrors;

let formHasError = form =>
  formHasGlobalError(form)
  || SMap.reduce(form.fields, false, (acc, _key, field) =>
       acc || List.length(field.errors) > 0
     );

let fieldsToJSON = form =>
  Obj.magic({
    "initialValues": getInitialValues(form),
    "values": getValues(form),
    "fields":
      SMap.reduce(
        form.fields,
        Js.Dict.empty(),
        (dict, key, _field) => {
          Js.Dict.set(
            dict,
            key,
            {
              "focus": hasFocus(key, form),
              "blur": isBlur(key, form),
              "dirty": isDirty(key, form),
              "hasError": hasError(key, form),
              "errors": getErrors(key, form),
            },
          );
          dict;
        },
      ),
  });