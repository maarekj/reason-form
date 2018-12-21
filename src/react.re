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

let formMetaEqual = (a: formMeta, b: formMeta) =>
  a.isDirty == b.isDirty
  && a.hasRootErrors == b.hasRootErrors
  && a.rootErrors == b.rootErrors
  && a.hasSubmitErrors == b.hasSubmitErrors
  && a.submitErrors == b.submitErrors
  && a.hasFieldErrors == b.hasFieldErrors
  && a.hasErrors == b.hasErrors
  && a.isSubmitting == b.isSubmitting
  && a.nbSubmits == b.nbSubmits
  && a.isSubmitSuccess == b.isSubmitSuccess;

let formMetaNotEqual = (a, b) => !formMetaEqual(a, b);

let metaEqual = (a: meta, b: meta) =>
  a.key == b.key
  && a.hasFocus == b.hasFocus
  && a.isBlur == b.isBlur
  && a.isDirty == b.isDirty
  && a.hasError == b.hasError
  && a.errors == b.errors;

let metaNotEqual = (a, b) => !metaEqual(a, b);

let createMeta = (field, form) => {
  key: Helper.key(field),
  hasFocus: Helper.hasFocus(field, form),
  isBlur: Helper.isBlur(field, form),
  isDirty: Helper.isDirty(field, form),
  hasError: Helper.hasError(field, form),
  errors: Helper.getErrors(field, form),
};

module type S = {
  type values;
  type form = Form.form(values);

  module Context: React_context.Context with type context := form;

  module WithFormMeta: {
    let make:
      (~pure: bool=?, ~render: formMeta => ReasonReact.reactElement, _) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };

  module WithForm: {
    let make:
      (
        ~form: Form.form(values),
        ~onChange: (form => form) => unit,
        ~onSubmit: (~dispatch: (form => form) => unit, ~form: Form.form(values)) =>
                   Js.Promise.t(Belt.Result.t('a, string)),
        ~render: (~handleSubmit: unit => unit) => ReasonReact.reactElement,
        array(ReasonReact.reactElement)
      ) =>
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

module Make = (FormConfig: Config) : (S with type values = FormConfig.values) => {
  type values = FormConfig.values;
  type form = Form.form(FormConfig.values);

  module Context =
    React_context.CreateContext({
      let debugName = "Form";
      type t = form;
      let value = FormConfig.initialForm;
    });

  module WithForm = {
    let component = ReasonReact.statelessComponent("WithFormControlled");
    let make = (~form: form, ~onChange, ~onSubmit, ~render, _children) => {
      ...component,
      render: _ => {
        let handleSubmit = () => {
          onChange(Form.startSubmit);
          onChange(Form.clearSubmitErrors);

          if (Form.formHasErrors(form)) {
            onChange(Form.stopSubmit);
          } else {
            onSubmit(~dispatch=onChange, ~form)
            |> Js.Promise.then_(value =>
                 switch (value) {
                 | Belt.Result.Ok(_) =>
                   onChange(Form.submitSuccess);
                   onChange(Form.stopSubmit);
                   Js.Promise.resolve();
                 | Error(error) =>
                   onChange(Form.addSubmitError(error));
                   onChange(Form.stopSubmit);
                   Js.Promise.resolve();
                 }
               )
            |> ignore;
          };
        };
        <Context.Provider value=form> {render(~handleSubmit)} </Context.Provider>;
      },
    };
  };

  module WithFormContainer = {
    type state = form;
    type action =
      | Dispatch(form => form);

    let component = ReasonReact.reducerComponent("ReactForm");
    let make = (~initialForm: form, ~onSubmit, ~render, _children) => {
      ...component,
      initialState: () => initialForm,
      reducer: (action: action, state: state) =>
        switch (action) {
        | Dispatch(action) => Update(action(state))
        },
      shouldUpdate: ({oldSelf, newSelf}) => !Form.Eq.form(oldSelf.state, newSelf.state),
      render: ({state, send}) => {
        let dispatch = action => send(Dispatch(action));

        <WithForm
          form=state
          onChange=dispatch
          onSubmit
          render={(~handleSubmit) => render(~dispatch, ~handleSubmit)}
        />;
      },
    };
  };

  module WithFormMeta = {
    module Consumer =
      Context.CreateConsumer({
        type t = formMeta;
      });

    let component = ReasonReact.statelessComponent("");
    let make = (~pure=?, ~render, _) => {
      ...component,
      render: _ => {
        let selector = (form, prevSelected): formMeta => {
          let newSelected = {
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

          switch (prevSelected) {
          | None => newSelected
          | Some(prev) => formMetaEqual(prev, newSelected) ? prev : newSelected
          };
        };
        <Consumer ?pure selector render />;
      },
    };
  };
};

module WithField = (React: S, Config: SType) => {
  module Consumer =
    React.Context.CreateConsumer({
      type t = (meta, Config.t);
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~pure=?, ~field: Field.t(_, _), ~render, _) => {
    ...component,
    render: _ => {
      let selector = (form, prev) => {
        let (newMeta, newValue) = (createMeta(`field(field), form), field.getValue(Form.getValues(form)));
        switch (prev) {
        | None => (newMeta, newValue)
        | Some((oldMeta, oldValue) as prev) =>
          oldValue === newValue && metaEqual(oldMeta, newMeta) ? prev : (newMeta, newValue)
        };
      };
      <Consumer
        ?pure
        selector
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
      type t = (meta, int, list(Row.t));
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~pure=?, ~field: FieldList.t(_, _, _), ~render, _) => {
    ...component,
    render: _ => {
      let selector = (form, prev) => {
        let newValues = Form.getValues(form);
        let (newMeta, newCount, newRows) = (
          createMeta(`list(field), form),
          field.count(newValues),
          field.getRows(newValues),
        );
        switch (prev) {
        | None => (newMeta, newCount, newRows)
        | Some((oldMeta, oldCount, _oldRows) as prev) =>
          oldCount == newCount && metaEqual(oldMeta, newMeta) ? prev : (newMeta, newCount, newRows)
        };
      };
      <Consumer
        ?pure
        selector
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
      type t = meta;
    });

  let component = ReasonReact.statelessComponent("");
  let make = (~pure=?, ~field, ~render, _) => {
    ...component,
    render: _ => {
      let selector = (form, prev) => {
        let newMeta = createMeta(`obj(field), form);
        switch (prev) {
        | None => newMeta
        | Some(prev) => metaEqual(prev, newMeta) ? prev : newMeta
        };
      };
      <Consumer
        ?pure
        selector
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
