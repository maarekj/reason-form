type metaField;

type metaFields;

type form('a);

let initializeForm:
  (
    ~initialValues: 'a,
    ~globalErrors: list(string)=?,
    ~onBlur: (string, form('a)) => form('a)=?,
    ~onFocus: (string, form('a)) => form('a)=?,
    ~onChangeValue: form('a) => form('a)=?,
    ~onValidate: form('a) => form('a)=?,
    unit
  ) =>
  form('a);

let mapFields: (form('a), metaFields => metaFields) => form('a);

let mapField: (form('a), metaField => metaField, string) => form('a);

let focus: (string, form('a)) => form('a);

let blur: (string, form('a)) => form('a);

let changeValues: (list(string), 'a, form('a)) => form('a);

let addGlobalError: (string, form('a)) => form('a);

let addError: (string, string, form('a)) => form('a);

let clearErrors: (string, form('a)) => form('a);

let hasFocus: (string, form('a)) => bool;

let isBlur: (string, form('a)) => bool;

let isDirty: (string, form('a)) => bool;

let hasError: (string, form('a)) => bool;

let getErrors: (string, form('a)) => list(string);

let getValues: form('a) => 'a;

let getInitialValues: form('a) => 'a;

let formIsDirty: form('a) => bool;

let formHasGlobalError: form('a) => bool;

let getGlobalErrors: form('a) => list(string);

let formHasError: form('a) => bool;

let fieldsToJSON: form('a) => Js.t({..});