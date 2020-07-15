type formMeta = {
  isDirty: bool,
  isAlreadyBlur: bool,
  hasRootErrors: bool,
  rootErrors: list(string),
  hasSubmitErrors: bool,
  submitErrors: list(string),
  hasFieldErrors: bool,
  hasErrors: bool,
  isSubmitting: bool,
  nbSubmits: int,
  isSubmitSuccess: bool,
};

type meta = {
  key: string,
  hasFocus: bool,
  isBlur: bool,
  isDirty: bool,
  isAlreadyBlur: bool,
  hasError: bool,
  errors: list(string),
};

let createFormMeta = form => {
  isDirty: Form.formIsDirty(form),
  isAlreadyBlur: Form.formIsAlreadyBlur(form),
  hasRootErrors: Form.formHasRootErrors(form),
  rootErrors: Form.getRootErrors(form),
  hasSubmitErrors: Form.formHasSubmitErrors(form),
  submitErrors: Form.getSubmitErrors(form),
  hasFieldErrors: Form.formHasFieldErrors(form),
  hasErrors: Form.formHasErrors(form),
  isSubmitting: Form.isSubmitting(form),
  nbSubmits: Form.getNbSubmits(form),
  isSubmitSuccess: Form.isSubmitSuccess(form),
};

let useListener = (wrap, createState) => {
  let (state, setState) = React.useState(() => createState(Wrap.content(wrap)));
  React.useEffect2(
    () => Some(wrap->Wrap.addListener(form => setState(_ => createState(form)))),
    (wrap, createState),
  );
  state;
};

let useFormMeta = wrap => {
  let createState = createFormMeta;
  useListener(wrap, createState);
};

let createMeta = (form, field) => {
  key: Helper.key(field),
  hasFocus: Helper.hasFocus(field, form),
  isBlur: Helper.isBlur(field, form),
  isDirty: Helper.isDirty(field, form),
  isAlreadyBlur: Helper.isAlreadyBlur(field, form),
  hasError: Helper.hasError(field, form),
  errors: Helper.getErrors(field, form),
};

let useMeta = (wrap, field) => {
  let createState = React.useCallback1(form => createMeta(form, field), [|field|]);
  useListener(wrap, createState);
};

let useValue = (wrap, field: Field.t('values, 'value)) => {
  let createState = React.useCallback1(form => field.getValue(Form.getValues(form)), [|field|]);
  useListener(wrap, createState);
};

let useListCount = (wrap, field) => {
  let createState = React.useCallback1(form => field.FieldList.count(Form.getValues(form)), [|field|]);
  useListener(wrap, createState);
};

let useMapCountKeys = (wrap, field) => {
  let createState = React.useCallback1(form => field.FieldMap.countKeys(Form.getValues(form)), [|field|]);
  useListener(wrap, createState);
};

let useMapAllKeys = (wrap, field) => {
  let createState = React.useCallback1(form => field.FieldMap.allKeys(Form.getValues(form)), [|field|]);
  useListener(wrap, createState);
};
