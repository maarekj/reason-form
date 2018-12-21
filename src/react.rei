module type Config = {
  type values;
  let initialForm: ReasonForm.Form.form(values);
};

module type SType = {type t;};
module MakeOption: (T: SType) => {type t = option(T.t);};

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

type fieldAction('values, 'v) = {
  focus: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  blur: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  clearErrors: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  addError: (string, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  setValue: ('v, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
};

type withField('values, 'value) = {
  value: 'value,
  meta,
  actions: fieldAction('values, 'value),
};

type listAction('values, 'v) = {
  focus: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  blur: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  clearErrors: ReasonForm.Form.form('values) => ReasonForm.Form.form('values),
  addError: (string, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  setList: (list('v), ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  update: (list('v) => list('v), ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  add: ('v, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  push: ('v, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  insert: (int, 'v, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
  remove: (int, ReasonForm.Form.form('values)) => ReasonForm.Form.form('values),
};

type withFieldList('values, 'value, 'row) = {
  meta,
  count: int,
  rows: list('row),
  actions: listAction('values, 'value),
};

type withFieldObject('values, 'value, 'fields) = {
  meta,
  fields: 'fields,
  actions: fieldAction('values, 'value),
};

let metaEqual: (meta, meta) => bool;
let metaNotEqual: (meta, meta) => bool;
let createMeta: (Helper.field('a, 'b, 'c, 'd), ReasonForm.Form.form('e)) => meta;

let formMetaEqual: (formMeta, formMeta) => bool;
let formMetaNotEqual: (formMeta, formMeta) => bool;

module type S = {
  type values;
  type form = Form.form(values);

  module Context: React_context.Context with type context := form;

  module WithFormMeta: {
    let make:
      (~pure: bool=?, ~render: formMeta => ReasonReact.reactElement, _) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };

  module WithFormContainer: {
    type action;
    let make:
      (
        ~initialForm: Form.form(values),
        ~onSubmit: (~dispatch: (form => form) => unit, ~form: Form.form(values)) =>
                   Js.Promise.t(Belt.Result.t('a, string)),
        ~render: (~dispatch: (form => form) => unit, ~handleSubmit: unit => unit) => ReasonReact.reactElement,
        array(ReasonReact.reactElement)
      ) =>
      ReasonReact.component(form, ReasonReact.noRetainedProps, action);
  };
};

module Make: (FormConfig: Config) => S with type values = FormConfig.values;

module WithField:
  (React: S, Config: SType) =>
   {
    let make:
      (
        ~pure: bool=?,
        ~field: ReasonForm.Field.t(React.values, Config.t),
        ~render: withField(React.values, Config.t) => ReasonReact.reactElement,
        'a
      ) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };

module WithFieldList:
  (React: S, Row: SType) =>
   {
    let make:
      (
        ~pure: bool=?,
        ~field: ReasonForm.FieldList.t(React.values, 'a, Row.t),
        ~render: withFieldList(React.values, 'a, Row.t) => ReasonReact.reactElement,
        'b
      ) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };

module WithFieldObject:
  (React: S) =>
   {
    let make:
      (
        ~pure: bool=?,
        ~field: ReasonForm.FieldObject.t('a, 'b, 'c),
        ~render: withFieldObject('a, 'b, 'c) => ReasonReact.reactElement,
        'd
      ) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };
