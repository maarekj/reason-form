module FieldErrors = {
  let component = ReasonReact.statelessComponent("FieldErrors");
  let make = (~meta: React.meta, ~reactForm: (module React.S), _children) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      <ReactForm.WithFormMeta
        render={
          ({nbSubmits}) =>
            (nbSubmits > 0 || meta.isDirty) && meta.errors != [] ?
              <div className="invalid-feedback" style={ReactDOMRe.Style.make(~display="block", ())}>
                {meta.errors |> Belt.List.map(_, ReasonReact.string) |> Belt.List.toArray |> ReasonReact.array}
              </div> :
              ReasonReact.null
        }
      />;
    },
  };
};

module FormErrors = {
  let component = ReasonReact.statelessComponent("FormErrors");
  let make = (~reactForm: (module React.S), _children) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      <ReactForm.WithFormMeta
        render={
          ({nbSubmits, hasRootErrors, hasSubmitErrors, rootErrors, submitErrors}) =>
            nbSubmits > 0 && (hasRootErrors || hasSubmitErrors) ?
              <div className="invalid-feedback" style={ReactDOMRe.Style.make(~display="block", ())}>
                {rootErrors |> Belt.List.map(_, ReasonReact.string) |> Belt.List.toArray |> ReasonReact.array}
                {submitErrors |> Belt.List.map(_, ReasonReact.string) |> Belt.List.toArray |> ReasonReact.array}
              </div> :
              ReasonReact.null
        }
      />;
    },
  };
};

module Input = {
  let component = ReasonReact.statelessComponent("Input");
  let make =
      (
        ~reactForm: (module React.S),
        ~dispatch,
        ~type_="text",
        ~className="form-control",
        ~toText,
        ~fromText,
        ~withField: React.withField(_, _),
        _children,
      ) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      let {React.meta, value, actions} = withField;
      <ReactForm.WithFormMeta
        render={
          ({nbSubmits}) =>
            <input
              type_
              className={className ++ " " ++ ((nbSubmits > 0 || meta.isDirty) && meta.hasError ? "is-invalid" : "")}
              value={toText(value)}
              onFocus={_event => dispatch(actions.focus)}
              onBlur={_event => dispatch(actions.blur)}
              onChange={
                event => {
                  let value = fromText(Helper.getValue(event));
                  dispatch(actions.setValue(value));
                }
              }
            />
        }
      />;
    },
  };
};

module SubmitButton = {
  let component = ReasonReact.statelessComponent("SubmitButton");
  let make = (~reactForm: (module React.S), ~text, ~submittingText, _children) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      <ReactForm.WithFormMeta
        render={
          ({isSubmitting, hasErrors}) =>
            <button type_="submit" className={"btn btn-primary " ++ (hasErrors || isSubmitting ? "disabled" : "")}>
              {ReasonReact.string(isSubmitting ? submittingText : text)}
            </button>
        }
      />;
    },
  };
};

module ResetButton = {
  let component = ReasonReact.statelessComponent("ResetButton");
  let make = (~reactForm: (module React.S), ~dispatch, ~initialForm, ~text, _children) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      <ReactForm.WithFormMeta
        render={
          ({isSubmitting}) =>
            <button
              className={"btn btn-light " ++ (isSubmitting ? "disabled" : "")}
              onClick={
                event => {
                  event->ReactEvent.Synthetic.preventDefault;
                  dispatch(_form => initialForm);
                }
              }>
              {ReasonReact.string(text)}
            </button>
        }
      />;
    },
  };
};

module Row = {
  let component = ReasonReact.statelessComponent("Row");
  let make =
      (
        ~reactForm: (module React.S),
        ~label,
        ~className="form-group",
        ~input,
        ~withField: React.withField(_, _),
        _children,
      ) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      Js.log("renderRow: " ++ label);
      <div className>
        <label> {ReasonReact.string(label)} </label>
        input
        <FieldErrors meta={withField.meta} reactForm />
      </div>;
    },
  };
};

module Obj = {
  let component = ReasonReact.statelessComponent("Object");
  let make =
      (
        ~reactForm: (module React.S),
        ~label,
        ~className="form-group",
        ~input,
        ~withField: React.withFieldObject(_, _, _),
        _children,
      ) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      Js.log("renderObject: " ++ label);
      <div className>
        <label> {ReasonReact.string(label)} </label>
        input
        <FieldErrors meta={withField.meta} reactForm />
      </div>;
    },
  };
};

module List = {
  let component = ReasonReact.statelessComponent("List");
  let make =
      (
        ~reactForm: (module React.S),
        ~onAdd,
        ~onRemove,
        ~label,
        ~renderInput,
        ~withField: React.withFieldList(_, _, _),
        _children,
      ) => {
    ...component,
    render: _ => {
      let (module ReactForm) = reactForm;
      Js.log("renderList: " ++ label);
      <div className="form-group">
        <label> {ReasonReact.string(label)} </label>
        <ul>
          {
            withField.rows
            |> Belt.List.mapWithIndex(_, (i, row) =>
                 <li key={string_of_int(i)}>
                   {renderInput(~row, ~index=i)}
                   <button
                     className="btn btn-light"
                     onClick={
                       event => {
                         ReactEvent.Synthetic.preventDefault(event);
                         onRemove(withField, i);
                       }
                     }>
                     {ReasonReact.string({js|❌|js})}
                   </button>
                 </li>
               )
            |> Belt.List.toArray
            |> ReasonReact.array
          }
        </ul>
        <FieldErrors reactForm meta={withField.meta} />
        <button
          className="btn btn-secondary"
          onClick={
            event => {
              ReactEvent.Synthetic.preventDefault(event);
              onAdd(withField);
            }
          }>
          {ReasonReact.string("Add +")}
        </button>
      </div>;
    },
  };
};
