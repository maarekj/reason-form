module SMap = Belt.Map.String

type metaField<'e> = {
  focus: bool,
  dirty: bool,
  alreadyBlur: bool,
  errors: list<'e>,
}

type metaFields<'e> = SMap.t<metaField<'e>>

let emptyField = {focus: false, dirty: false, alreadyBlur: false, errors: list{}}

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

module Eq = {
  let metaField = (a: metaField<'e>, b: metaField<'e>) =>
    a === b || (a.focus == b.focus && (a.dirty == b.dirty && a.errors == b.errors))
  let metaFields = (a: metaFields<'e>, b: metaFields<'e>) => a === b || SMap.eq(a, b, metaField)
  let form = (a: form<'v, 'e>, b: form<'v, 'e>) =>
    a === b ||
      (metaFields(a.fields, b.fields) &&
      (a.rootErrors == b.rootErrors &&
      (a.submitErrors == b.submitErrors &&
        (a.submitSuccess == b.submitSuccess &&
        (a.onValidate === b.onValidate &&
          (a.onBlur === b.onBlur &&
          (a.onFocus === b.onFocus &&
            (a.onChangeValue === b.onChangeValue &&
            (a.submitting == b.submitting &&
              (a.nbSubmits == b.nbSubmits &&
              (a.initialValues === b.initialValues && a.values === b.values)))))))))))
}

let listIsEmpty = l =>
  switch l {
  | list{} => true
  | _ => false
  }

let initializeForm = (
  ~initialValues,
  ~rootErrors=list{},
  ~submitErrors=list{},
  ~onBlur=(_key, form) => form,
  ~onFocus=(_key, form) => form,
  ~onChangeValue=(~keys as _, ~oldForm as _, ~newForm) => newForm,
  ~onValidate=form => form,
  (),
) => {
  let form = {
    fields: SMap.empty,
    rootErrors: rootErrors,
    submitErrors: submitErrors,
    submitSuccess: false,
    onValidate: onValidate,
    onBlur: onBlur,
    onFocus: onFocus,
    onChangeValue: onChangeValue,
    initialValues: initialValues,
    submitting: false,
    nbSubmits: 0,
    values: initialValues,
  }
  form |> onValidate
}

let mapFields = (form, f) => {
  let fields = f(form.fields)
  {...form, fields: fields}
}

let mapField = (form, f, key) =>
  mapFields(form, fields => {
    let field = fields->SMap.get(key)->Belt.Option.getWithDefault(emptyField)
    let newField = f(field)
    SMap.set(fields, key, newField)
  })

let getField = (key, form) =>
  SMap.get(form.fields, key) |> Belt.Option.getWithDefault(_, emptyField)

let focus = (key, form) =>
  mapField(
    form,
    field => field.focus == true ? field : {...field, focus: true},
    key,
  ) |> form.onFocus(key)

let blur = (key, form) =>
  mapField(
    form,
    field =>
      field.focus == false && field.alreadyBlur === true
        ? field
        : {...field, focus: false, alreadyBlur: true},
    key,
  ) |> form.onBlur(key)

let addRootError = (error, form) => {...form, rootErrors: list{error, ...form.rootErrors}}

let addSubmitError = (error, form) => {...form, submitErrors: list{error, ...form.submitErrors}}

let addError = (key, error, form) =>
  mapField(form, field => {...field, errors: list{error, ...field.errors}}, key)

let clearErrors = (key, form) =>
  mapField(form, field => listIsEmpty(field.errors) ? field : {...field, errors: list{}}, key)

let clearRootErrors = form => listIsEmpty(form.rootErrors) ? form : {...form, rootErrors: list{}}

let clearSubmitErrors = form =>
  listIsEmpty(form.submitErrors) ? form : {...form, submitErrors: list{}}

let clearAllFieldsErrors = form =>
  mapFields(form, fields => {
    let keys = SMap.keysToArray(fields)
    Belt.Array.reduce(keys, fields, (fields, key) => {
      let field = SMap.get(fields, key)
      field
      ->Belt.Option.map(field =>
        listIsEmpty(field.errors) ? fields : fields->SMap.set(key, {...field, errors: list{}})
      )
      ->Belt.Option.getWithDefault(fields)
    })
  })

let changeValues = (keys, values, form) => {
  let oldForm = form
  let form = {...form, values: values}
  List.fold_left(
    (form, key) =>
      mapField(form, field => field.dirty == true ? field : {...field, dirty: true}, key),
    form,
    keys,
  )
  |> form.onChangeValue(~keys, ~oldForm, ~newForm=_)
  |> clearRootErrors
  |> clearAllFieldsErrors
  |> form.onValidate
}

let hasFocus = (key, form) => getField(key, form).focus

let isBlur = (key, form) => !getField(key, form).focus

let isDirty = (key, form) => getField(key, form).dirty

let isAlreadyBlur = (key, form) => getField(key, form).alreadyBlur

let hasError = (key, form) => List.length(getField(key, form).errors) > 0

let getErrors = (key, form) => getField(key, form).errors

let getValues = form => form.values

let getInitialValues = form => form.initialValues

let formIsDirty = form => SMap.reduce(form.fields, false, (acc, _key, field) => acc || field.dirty)

let formIsAlreadyBlur = form =>
  SMap.reduce(form.fields, false, (acc, _key, field) => acc || field.alreadyBlur)

let formHasRootErrors = form => List.length(form.rootErrors) > 0

let getRootErrors = form => form.rootErrors

let formHasSubmitErrors = form => List.length(form.submitErrors) > 0

let getSubmitErrors = form => form.submitErrors

let formHasFieldErrors = form =>
  SMap.reduce(form.fields, false, (acc, _key, field) => acc || List.length(field.errors) > 0)

let formHasErrors = form => formHasRootErrors(form) || formHasFieldErrors(form)

let startSubmit = form => {...form, submitting: true, nbSubmits: form.nbSubmits + 1}

let stopSubmit = form => form.submitting == false ? form : {...form, submitting: false}

let submitSuccess = form => form.submitSuccess == true ? form : {...form, submitSuccess: true}

let isSubmitSuccess = form => form.submitSuccess

let isSubmitting = form => form.submitting

let getNbSubmits = form => form.nbSubmits
