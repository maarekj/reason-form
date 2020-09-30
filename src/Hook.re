type formMeta('e) = {
  isDirty: bool,
  isAlreadyBlur: bool,
  hasRootErrors: bool,
  rootErrors: list('e),
  hasSubmitErrors: bool,
  submitErrors: list('e),
  hasFieldErrors: bool,
  hasErrors: bool,
  isSubmitting: bool,
  nbSubmits: int,
  isSubmitSuccess: bool,
};

type meta('e) = {
  key: string,
  hasFocus: bool,
  isBlur: bool,
  isDirty: bool,
  isAlreadyBlur: bool,
  hasError: bool,
  errors: list('e),
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

let createMeta = (form, field: Field.t(_, _)) => {
  key: field.key,
  hasFocus: Form.hasFocus(field.key, form),
  isBlur: Form.isBlur(field.key, form),
  isDirty: Form.isDirty(field.key, form),
  isAlreadyBlur: Form.isAlreadyBlur(field.key, form),
  hasError: Form.hasError(field.key, form),
  errors: Form.getErrors(field.key, form),
};

let useMeta = (wrap, field) => {
  let createState = React.useCallback1(form => createMeta(form, field), [|field|]);
  useListener(wrap, createState);
};

let useFormValues = (wrap, mapper) => {
  let createState = React.useCallback1(form => mapper(Form.getValues(form)), [|mapper|]);
  useListener(wrap, createState);
};

let useValue = (wrap, field: Field.t(_, _)) => {
  let createState = React.useCallback1(form => field.getValue(Form.getValues(form)), [|field|]);
  useListener(wrap, createState);
};

let useListCount = (wrap, field: Field.t(_, _)) => {
  let createState =
    React.useCallback1(
      form => {
        let list = field.getValue(Form.getValues(form));
        list->Belt.List.length;
      },
      [|field|],
    );
  useListener(wrap, createState);
};

let useStringMapCountKeys = (wrap, field: Field.t(_, _)) => {
  let createState =
    React.useCallback1(
      form => {
        let map = field.getValue(Form.getValues(form));
        map->Belt.Map.String.keysToArray->Belt.Array.length;
      },
      [|field|],
    );
  useListener(wrap, createState);
};

let useStringMapAllKeys = (wrap, field: Field.t(_, _)) => {
  let createState =
    React.useCallback1(
      form => {
        let map = field.getValue(Form.getValues(form));
        map->Belt.Map.String.keysToArray;
      },
      [|field|],
    );
  let allKeys = useListener(wrap, createState);
  React.useMemo1(() => allKeys, [|allKeys->Belt.Array.reduce("", (++))|]);
};
