type metaField;

type metaFields;

type form('a);

let initializeForm:
  (
    ~initialValues: 'a,
    ~rootErrors: list(string)=?,
    ~submitErrors: list(string)=?,
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

let addRootError: (string, form('a)) => form('a);

let addSubmitError: (string, form('a)) => form('a);

let addError: (string, string, form('a)) => form('a);

let clearErrors: (string, form('a)) => form('a);

let clearRootErrors: form('a) => form('a);

let clearSubmitErrors: form('a) => form('a);

let hasFocus: (string, form('a)) => bool;

let isBlur: (string, form('a)) => bool;

let isDirty: (string, form('a)) => bool;

let hasError: (string, form('a)) => bool;

let getErrors: (string, form('a)) => list(string);

let getValues: form('a) => 'a;

let getInitialValues: form('a) => 'a;

let formIsDirty: form('a) => bool;

let formHasRootErrors: form('a) => bool;

let getRootErrors: form('a) => list(string);

let formHasSubmitErrors: form('a) => bool;

let getSubmitErrors: form('a) => list(string);

let formHasErrors: form('a) => bool;

let startSubmit: form('a) => form('a);

let stopSubmit: form('a) => form('a);

let isSubmitting: form('a) => bool;

let getNbSubmits: form('a) => int;

let fieldsToJSON: form('a) => Js.t({..});
