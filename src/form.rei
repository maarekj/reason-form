type metaField('e);

type metaFields('e);

type form('v, 'e);

let initializeForm:
  (
    ~initialValues: 'v,
    ~rootErrors: list('e)=?,
    ~submitErrors: list('e)=?,
    ~onBlur: (string, form('v, 'e)) => form('v, 'e)=?,
    ~onFocus: (string, form('v, 'e)) => form('v, 'e)=?,
    ~onChangeValue: form('v, 'e) => form('v, 'e)=?,
    ~onValidate: form('v, 'e) => form('v, 'e)=?,
    unit
  ) =>
  form('v, 'e);

module Eq: {
  let metaField: (metaField('e), metaField('e)) => bool;
  let metaFields: (metaFields('e), metaFields('e)) => bool;
  let form: (form('v, 'e), form('v, 'e)) => bool;
};

let mapFields: (form('v, 'e), metaFields('e) => metaFields('e)) => form('v, 'e);

let mapField: (form('v, 'e), metaField('e) => metaField('e), string) => form('v, 'e);

let getField: (string, form('v, 'e)) => metaField('e);

let focus: (string, form('v, 'e)) => form('v, 'e);

let blur: (string, form('v, 'e)) => form('v, 'e);

let changeValues: (list(string), 'v, form('v, 'e)) => form('v, 'e);

let addRootError: ('e, form('v, 'e)) => form('v, 'e);

let addSubmitError: ('e, form('v, 'e)) => form('v, 'e);

let addError: (string, 'e, form('v, 'e)) => form('v, 'e);

let clearErrors: (string, form('v, 'e)) => form('v, 'e);

let clearRootErrors: form('v, 'e) => form('v, 'e);

let clearSubmitErrors: form('v, 'e) => form('v, 'e);

let hasFocus: (string, form('v, 'e)) => bool;

let isBlur: (string, form('v, 'e)) => bool;

let isDirty: (string, form('v, 'e)) => bool;

let hasError: (string, form('v, 'e)) => bool;

let getErrors: (string, form('v, 'e)) => list('e);

let getValues: form('v, 'e) => 'v;

let getInitialValues: form('v, 'e) => 'v;

let formIsDirty: form('v, 'e) => bool;

let formHasRootErrors: form('v, 'e) => bool;

let getRootErrors: form('v, 'e) => list('e);

let formHasSubmitErrors: form('v, 'e) => bool;

let getSubmitErrors: form('v, 'e) => list('e);

let formHasFieldErrors: form('v, 'e) => bool;

let formHasErrors: form('v, 'e) => bool;

let startSubmit: form('v, 'e) => form('v, 'e);

let stopSubmit: form('v, 'e) => form('v, 'e);

let submitSuccess: form('v, 'e) => form('v, 'e);

let isSubmitSuccess: form('v, 'e) => bool;

let isSubmitting: form('v, 'e) => bool;

let getNbSubmits: form('v, 'e) => int;
