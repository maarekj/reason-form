module type Config = {
  type values;
  let initialForm: Reasonform_form.form(values);
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
  focus: Reasonform_form.form('values) => Reasonform_form.form('values),
  blur: Reasonform_form.form('values) => Reasonform_form.form('values),
  clearErrors: Reasonform_form.form('values) => Reasonform_form.form('values),
  addError: (string, Reasonform_form.form('values)) => Reasonform_form.form('values),
  setValue: ('v, Reasonform_form.form('values)) => Reasonform_form.form('values),
};

type withField('values, 'value) = {
  value: 'value,
  meta,
  actions: fieldAction('values, 'value),
};

type listAction('values, 'v) = {
  focus: Reasonform_form.form('values) => Reasonform_form.form('values),
  blur: Reasonform_form.form('values) => Reasonform_form.form('values),
  clearErrors: Reasonform_form.form('values) => Reasonform_form.form('values),
  addError: (string, Reasonform_form.form('values)) => Reasonform_form.form('values),
  setList: (list('v), Reasonform_form.form('values)) => Reasonform_form.form('values),
  update: (list('v) => list('v), Reasonform_form.form('values)) => Reasonform_form.form('values),
  add: ('v, Reasonform_form.form('values)) => Reasonform_form.form('values),
  push: ('v, Reasonform_form.form('values)) => Reasonform_form.form('values),
  insert: (int, 'v, Reasonform_form.form('values)) => Reasonform_form.form('values),
  remove: (int, Reasonform_form.form('values)) => Reasonform_form.form('values),
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
let createMeta: (Reasonform_helper.field('a, 'b, 'c, 'd), Reasonform_form.form('e)) => meta;

module type WithFormMeta = {
  let make:
    (~render: formMeta => ReasonReact.reactElement, 'a) =>
    ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
};

module type S = {
  type values;
  type form = Reasonform_form.form(values);
  module Context: Reasonform_reactContext.Context with type context := form;

  module WithFormMeta: WithFormMeta;

  type action;
  let make:
    (
      ~initialForm: Reasonform_form.form(values),
      ~onSubmit: (~dispatch: (form => form) => unit, ~form: Reasonform_form.form(values)) =>
                 Js.Promise.t(Belt.Result.t('a, string)),
      ~render: (~dispatch: (form => form) => unit, ~handleSubmit: unit => unit) => ReasonReact.reactElement,
      array(ReasonReact.reactElement)
    ) =>
    ReasonReact.component(form, ReasonReact.noRetainedProps, action);
};

module Make: (FormConfig: Config) => S with type values = FormConfig.values;

module WithField:
  (React: S, Config: SType) =>
   {
    let make:
      (
        ~field: Reasonform_field.t(React.values, Config.t),
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
        ~field: Reasonform_fieldList.t(React.values, 'a, Row.t),
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
        ~field: Reasonform_fieldObject.t('a, 'b, 'c),
        ~render: withFieldObject('a, 'b, 'c) => ReasonReact.reactElement,
        'd
      ) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };
