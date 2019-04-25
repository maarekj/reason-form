type formMeta = {
  isDirty: bool,
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
  hasError: bool,
  errors: list(string),
};

let createFormMeta = form => {
  isDirty: Form.formIsDirty(form),
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

let useFormMeta = wrap => {
  let (meta, setMeta) = React.useState(() => createFormMeta(Wrap.content(wrap)));
  React.useEffect1(() => wrap->Wrap.addListener(form => setMeta(_ => createFormMeta(form))), [|wrap|]);
  meta;
};

let createMeta = (form, field) => {
  key: Helper.key(field),
  hasFocus: Helper.hasFocus(field, form),
  isBlur: Helper.isBlur(field, form),
  isDirty: Helper.isDirty(field, form),
  hasError: Helper.hasError(field, form),
  errors: Helper.getErrors(field, form),
};

let useMeta = (wrap, field) => {
  let (meta, setMeta) = React.useState(() => createMeta(Wrap.content(wrap), field));

  React.useEffect2(() => wrap->Wrap.addListener(form => setMeta(_ => createMeta(form, field))), (wrap, field));

  meta;
};

let useValue = (wrap, field: Field.t('values, 'value)) => {
  let (value, setValue) =
    React.useState(() => {
      let form = Wrap.content(wrap);
      field.getValue(Form.getValues(form));
    });

  React.useEffect2(
    () => wrap->Wrap.addListener(form => setValue(_ => field.getValue(Form.getValues(form)))),
    (wrap, field),
  );
  value;
};

let useListCount = (wrap, field) => {
  let (count, setCount) =
    React.useState(() => {
      let form = Wrap.content(wrap);
      let values = Form.getValues(form);
      field.FieldList.count(values);
    });

  React.useEffect2(
    () =>
      wrap->Wrap.addListener(form => {
        let values = Form.getValues(form);
        setCount(_ => field.FieldList.count(values));
      }),
    (wrap, field),
  );

  count;
};
