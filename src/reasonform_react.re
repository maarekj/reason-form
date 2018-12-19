module Form = Reasonform_form;
module Field = Reasonform_field;
module FieldObject = Reasonform_fieldObject;
module FieldList = Reasonform_fieldList;
module Helper = Reasonform_helper;
module ReactContext = Reasonform_reactContext;

module type Config = {
  type values;
  let initialForm: Form.form(values);
};

module type SType = {type t;};
module MakeOption = (T: SType) : (SType with type t = option(T.t)) => {
  type t = option(T.t);
};

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
  focus: Form.form('values) => Form.form('values),
  blur: Form.form('values) => Form.form('values),
  clearErrors: Form.form('values) => Form.form('values),
  addError: (string, Form.form('values)) => Form.form('values),
  setValue: ('v, Form.form('values)) => Form.form('values),
};

type withField('values, 'value) = {
  value: 'value,
  meta,
  actions: fieldAction('values, 'value),
};

type listAction('values, 'v) = {
  focus: Form.form('values) => Form.form('values),
  blur: Form.form('values) => Form.form('values),
  clearErrors: Form.form('values) => Form.form('values),
  addError: (string, Form.form('values)) => Form.form('values),
  setList: (list('v), Form.form('values)) => Form.form('values),
  update: (list('v) => list('v), Form.form('values)) => Form.form('values),
  add: ('v, Form.form('values)) => Form.form('values),
  push: ('v, Form.form('values)) => Form.form('values),
  insert: (int, 'v, Form.form('values)) => Form.form('values),
  remove: (int, Form.form('values)) => Form.form('values),
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

let metaEqual = (a: meta, b: meta) =>
  a.key == b.key
  && a.hasFocus == b.hasFocus
  && a.isBlur == b.isBlur
  && a.isDirty == b.isDirty
  && a.hasError == b.hasError
  && a.errors == b.errors;

let metaNotEqual = (a: meta, b: meta) => !metaEqual(a, b);

let createMeta = (field, form) => {
  key: Helper.key(field),
  hasFocus: Helper.hasFocus(field, form),
  isBlur: Helper.isBlur(field, form),
  isDirty: Helper.isDirty(field, form),
  hasError: Helper.hasError(field, form),
  errors: Helper.getErrors(field, form),
};

module type WithFormMeta = {
  let make:
    (~render: formMeta => ReasonReact.reactElement, _) =>
    ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
};

module type S = {
  type values;
  type form = Form.form(values);

  module Context: ReactContext.Context with type context := form;

  module WithFormMeta: WithFormMeta;

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

module Make = (FormConfig: Config) : (S with type values = FormConfig.values) => {
  type values = FormConfig.values;
  type form = Form.form(FormConfig.values);

  module Context =
    ReactContext.CreateContext({
      let debugName = "Form";
      type t = form;
      let value = FormConfig.initialForm;
    });

  module WithFormMeta: WithFormMeta = {
    module Consumer =
      Context.CreateConsumer({
        type context = form;
        type t = formMeta;
      });

    let component = ReasonReact.statelessComponent("");
    let make = (~render, _) => {
      ...component,
      render: _ => {
        let selector = form => {
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
        <Consumer selector shouldUpdate={(_, _) => true} render />;
      },
    };
  };

  type state = form;
  type action =
    | Dispatch(form => form)
    | GetForm(form => unit);

  let component = ReasonReact.reducerComponent("ReactForm");
  let make = (~initialForm: form, ~onSubmit, ~render, _children) => {
    ...component,
    initialState: () => initialForm,
    reducer: (action: action, state: state) =>
      switch (action) {
      | Dispatch(action) => Update(action(state))
      | GetForm(callback) => SideEffects((({state}) => callback(state)))
      },
    render: ({state, send}) => {
      let dispatch = action => send(Dispatch(action));
      let getForm = callback => send(GetForm(callback));
      let handleSubmit = () => {
        dispatch(Form.startSubmit);
        dispatch(Form.clearSubmitErrors);
        getForm(form =>
          if (Form.formHasErrors(form)) {
            dispatch(Form.stopSubmit);
          } else {
            onSubmit(~dispatch, ~form)
            |> Js.Promise.then_(value =>
                 switch (value) {
                 | Belt.Result.Ok(_) =>
                   dispatch(Form.submitSuccess);
                   dispatch(Form.stopSubmit);
                   Js.Promise.resolve();
                 | Error(error) =>
                   dispatch(Form.addSubmitError(error));
                   dispatch(Form.stopSubmit);
                   Js.Promise.resolve();
                 }
               )
            |> ignore;
          }
        );
      };
      <Context.Provider value=state> ...{render(~dispatch, ~handleSubmit)} </Context.Provider>;
    },
  };
};

module WithField = (React: S, Config: SType) => {
  module Consumer =
    React.Context.CreateConsumer({
      type context = React.form;
      type t = (meta, Config.t);
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~field: Field.t(_, _), ~render, _) => {
    ...component,
    render: _ => {
      let selector = form => (createMeta(`field(field), form), field.getValue(Form.getValues(form)));
      <Consumer
        selector
        shouldUpdate=(!=)
        render={
          ((meta, value)) =>
            render({
              meta,
              value,
              actions: {
                focus: form => Helper.focus(`field(field), form),
                blur: form => Helper.blur(`field(field), form),
                clearErrors: form => Helper.clearErrors(`field(field), form),
                addError: (message, form) => Helper.addError(`field(field), message, form),
                setValue: (value, form) => {
                  let value = field.setValue(value, Form.getValues(form));
                  Form.changeValues([field.key], value, form);
                },
              },
            })
        }
      />;
    },
  };
};

module WithFieldList = (React: S, Row: SType) => {
  module Consumer =
    React.Context.CreateConsumer({
      type context = React.form;
      type t = (meta, int, list(Row.t));
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~field: FieldList.t(_, _, _), ~render, _) => {
    ...component,
    render: _ => {
      let selector = form => {
        let meta = createMeta(`list(field), form);
        let values = Form.getValues(form);
        (meta, field.count(values), field.getRows(values));
      };
      <Consumer
        selector
        shouldUpdate={((aMeta, aCount, _), (bMeta, bCount, _)) => aCount != bCount || metaNotEqual(aMeta, bMeta)}
        render={
          ((meta, count, rows)) =>
            render({
              meta,
              count,
              rows,
              actions: {
                focus: form => Helper.focus(`list(field), form),
                blur: form => Helper.blur(`list(field), form),
                clearErrors: form => Helper.clearErrors(`list(field), form),
                addError: (message, form) => Helper.addError(`list(field), message, form),
                setList: (list, form) => {
                  let list = field.setList(list, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
                add: (value, form) => {
                  let list = field.add(value, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
                push: (value, form) => {
                  let list = field.push(value, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
                insert: (index, value, form) => {
                  let list = field.insert(index, value, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
                remove: (index, form) => {
                  let list = field.remove(index, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
                update: (updater, form) => {
                  let list = field.update(updater, Form.getValues(form));
                  Form.changeValues([field.key], list, form);
                },
              },
            })
        }
      />;
    },
  };
};

module WithFieldObject = (React: S) => {
  module Consumer =
    React.Context.CreateConsumer({
      type context = React.form;
      type t = meta;
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~field, ~render, _) => {
    ...component,
    render: _ => {
      let selector = form => createMeta(`obj(field), form);
      <Consumer
        selector
        shouldUpdate=metaNotEqual
        render={
          meta =>
            render({
              meta,
              fields: field.fields,
              actions: {
                focus: form => Helper.focus(`obj(field), form),
                blur: form => Helper.blur(`obj(field), form),
                clearErrors: form => Helper.clearErrors(`obj(field), form),
                addError: (message, form) => Helper.addError(`obj(field), message, form),
                setValue: (value, form) => {
                  let value = field.setObject(value, Form.getValues(form));
                  Form.changeValues([field.key], value, form);
                },
              },
            })
        }
      />;
    },
  };
};
