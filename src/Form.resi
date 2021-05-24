type metaField<'e> = {
  focus: bool,
  dirty: bool,
  alreadyBlur: bool,
  errors: list<'e>,
}

type metaFields<'e> = Belt.Map.String.t<metaField<'e>>

type rec form<'values, 'error> = {
  fields: metaFields<'error>,
  rootErrors: list<'error>,
  submitErrors: list<'error>,
  submitSuccess: bool,
  onValidate: form<'values, 'error> => form<'values, 'error>,
  onBlur: (string, form<'values, 'error>) => form<'values, 'error>,
  onFocus: (string, form<'values, 'error>) => form<'values, 'error>,
  onChangeValue: (
    ~keys: list<string>,
    ~oldForm: form<'values, 'error>,
    ~newForm: form<'values, 'error>,
  ) => form<'values, 'error>,
  submitting: bool,
  nbSubmits: int,
  initialValues: 'values,
  values: 'values,
}

type t<'values, 'error> = form<'values, 'error>

let initializeForm: (
  ~initialValues: 'v,
  ~rootErrors: list<'e>=?,
  ~submitErrors: list<'e>=?,
  ~onBlur: (string, form<'v, 'e>) => form<'v, 'e>=?,
  ~onFocus: (string, form<'v, 'e>) => form<'v, 'e>=?,
  ~onChangeValue: (
    ~keys: list<string>,
    ~oldForm: form<'v, 'e>,
    ~newForm: form<'v, 'e>,
  ) => form<'v, 'e>=?,
  ~onValidate: form<'v, 'e> => form<'v, 'e>=?,
  unit,
) => form<'v, 'e>

module Eq: {
  let metaField: (metaField<'e>, metaField<'e>) => bool
  let metaFields: (metaFields<'e>, metaFields<'e>) => bool
  let form: (form<'v, 'e>, form<'v, 'e>) => bool
}

let mapFields: (form<'v, 'e>, metaFields<'e> => metaFields<'e>) => form<'v, 'e>

let mapField: (form<'v, 'e>, metaField<'e> => metaField<'e>, string) => form<'v, 'e>

let getField: (string, form<'v, 'e>) => metaField<'e>

let focus: (string, form<'v, 'e>) => form<'v, 'e>

let blur: (string, form<'v, 'e>) => form<'v, 'e>

let changeValues: (list<string>, 'v, form<'v, 'e>) => form<'v, 'e>

let addRootError: ('e, form<'v, 'e>) => form<'v, 'e>

let addSubmitError: ('e, form<'v, 'e>) => form<'v, 'e>

let addError: (string, 'e, form<'v, 'e>) => form<'v, 'e>

let clearErrors: (string, form<'v, 'e>) => form<'v, 'e>

let clearRootErrors: form<'v, 'e> => form<'v, 'e>

let clearSubmitErrors: form<'v, 'e> => form<'v, 'e>

let hasFocus: (string, form<'v, 'e>) => bool

let isBlur: (string, form<'v, 'e>) => bool

let isDirty: (string, form<'v, 'e>) => bool

let isAlreadyBlur: (string, form<'v, 'e>) => bool

let hasError: (string, form<'v, 'e>) => bool

let getErrors: (string, form<'v, 'e>) => list<'e>

let getValues: form<'v, 'e> => 'v

let getInitialValues: form<'v, 'e> => 'v

let formIsDirty: form<'v, 'e> => bool

let formIsAlreadyBlur: form<'v, 'e> => bool

let formHasRootErrors: form<'v, 'e> => bool

let getRootErrors: form<'v, 'e> => list<'e>

let formHasSubmitErrors: form<'v, 'e> => bool

let getSubmitErrors: form<'v, 'e> => list<'e>

let formHasFieldErrors: form<'v, 'e> => bool

let formHasErrors: form<'v, 'e> => bool

let startSubmit: form<'v, 'e> => form<'v, 'e>

let stopSubmit: form<'v, 'e> => form<'v, 'e>

let submitSuccess: form<'v, 'e> => form<'v, 'e>

let isSubmitSuccess: form<'v, 'e> => bool

let isSubmitting: form<'v, 'e> => bool

let getNbSubmits: form<'v, 'e> => int
