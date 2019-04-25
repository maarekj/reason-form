let normalizeId = id => {
  let reg = [%bs.re "/[^-_a-zA-Z0-9]/g"];
  id |> Js.String.replaceByRe(reg, "-");
};

module FieldErrors = {
  [@react.component]
  let make = (~wrap, ~field) => {
    let {Hook.nbSubmits} = Hook.useFormMeta(wrap);
    let {Hook.isDirty, errors} = Hook.useMeta(wrap, field);

    React.useMemo3(
      () =>
        (nbSubmits > 0 || isDirty) && Belt.List.length(errors) != 0
          ? <div className="invalid-feedback" style={ReactDOMRe.Style.make(~display="block", ())}>
              {errors |> Belt.List.map(_, React.string) |> Belt.List.toArray |> React.array}
            </div>
          : React.null,
      (nbSubmits, isDirty, errors),
    );
  };
};

module FormErrors = {
  [@react.component]
  let make = (~wrap) => {
    let {Hook.nbSubmits, hasRootErrors, hasSubmitErrors, rootErrors, submitErrors} = Hook.useFormMeta(wrap);

    React.useMemo5(
      () =>
        nbSubmits > 0 && (hasRootErrors || hasSubmitErrors)
          ? <div className="invalid-feedback" style={ReactDOMRe.Style.make(~display="block", ())}>
              {rootErrors |> Belt.List.map(_, React.string) |> Belt.List.toArray |> React.array}
              {submitErrors |> Belt.List.map(_, React.string) |> Belt.List.toArray |> React.array}
            </div>
          : ReasonReact.null,
      (nbSubmits, hasRootErrors, hasSubmitErrors, rootErrors, submitErrors),
    );
  };
};

[@bs.module "react"]
external useMemo8: ([@bs.uncurry] (unit => 'any), ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)) => 'any = "useMemo";

[@bs.module "react"]
external useMemo9: ([@bs.uncurry] (unit => 'any), ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)) => 'any = "useMemo";

module Input = {
  [@react.component]
  let make = (~wrap, ~type_="text", ~id=?, ~className="form-control", ~toText, ~fromText, ~field) => {
    let value = Hook.useValue(wrap, field);
    let fieldField = React.useMemo1(() => `field(field), [|field|]);
    let {Hook.nbSubmits} = Hook.useFormMeta(wrap);
    let {Hook.isDirty, hasError, key} = Hook.useMeta(wrap, fieldField);

    useMemo9(
      () => {
        Js.log("render input");

        let focus = Helper.focus(`field(field));
        let blur = Helper.blur(`field(field));
        let setValue = (value, form) => {
          let value = field.setValue(value, Form.getValues(form));
          Form.changeValues([field.key], value, form);
        };

        let id =
          switch (id) {
          | Some(id) => id
          | None =>
            let prefixId = Wrap.id(wrap);
            normalizeId(prefixId ++ "-" ++ key);
          };

        <input
          type_
          id
          className={className ++ " " ++ ((nbSubmits > 0 || isDirty) && hasError ? "is-invalid" : "")}
          value={toText(value)}
          onFocus={_event => wrap->Wrap.dispatch(focus)}
          onBlur={_event => wrap->Wrap.dispatch(blur)}
          onChange={event => {
            let value = fromText(Helper.getValue(event));
            wrap->Wrap.dispatch(setValue(value));
          }}
        />;
      },
      (value, nbSubmits, isDirty, hasError, key, className, toText, fromText, id),
    );
  };
};

module Choice = {
  type choice('v) = {
    value: 'v,
    string,
    label: string,
  };

  [@react.component]
  let make = (~wrap, ~expanded=false, ~className="form-control", ~id=?, ~choices, ~field) => {
    let value = Hook.useValue(wrap, field);
    let fieldField = React.useMemo1(() => `field(field), [|field|]);
    let {Hook.nbSubmits} = Hook.useFormMeta(wrap);
    let {Hook.isDirty, hasError, key} = Hook.useMeta(wrap, fieldField);

    useMemo9(
      () => {
        let focus = Helper.focus(`field(field));
        let blur = Helper.blur(`field(field));
        let setValue = (value, form) => {
          let value = field.setValue(value, Form.getValues(form));
          Form.changeValues([field.key], value, form);
        };

        let id =
          switch (id) {
          | Some(id) => id
          | None =>
            let prefixId = Wrap.id(wrap);
            normalizeId(prefixId ++ "-" ++ key);
          };

        let toString = value => {
          let choice = choices->Belt.List.getByU((. choice) => choice.value == value);
          switch (choice) {
          | Some({string}) => string
          | None => ""
          };
        };

        let fromString = string => {
          let choice = choices->Belt.List.getByU((. choice) => choice.string == string);
          switch (choice) {
          | Some({value}) => Some(value)
          | None => None
          };
        };

        let string = toString(value);

        Js.log("render choice");

        switch (expanded) {
        | false =>
          <select
            id
            className={className ++ " " ++ ((nbSubmits > 0 || isDirty) && hasError ? "is-invalid" : "")}
            value=string
            onFocus={_event => wrap->Wrap.dispatch(focus)}
            onBlur={_event => wrap->Wrap.dispatch(blur)}
            onChange={event => {
              let value = fromString(Helper.getValue(event));
              switch (value) {
              | Some(value) => wrap->Wrap.dispatch(setValue(value))
              | None => ()
              };
            }}>
            {choices
             ->Belt.List.mapU((. choice) =>
                 <option key={choice.string} value={choice.string}> {React.string(choice.label)} </option>
               )
             ->Belt.List.toArray
             ->React.array}
          </select>
        | true =>
          let onChange = (value, _event) => wrap->Wrap.dispatch(setValue(value));
          <div className={(nbSubmits > 0 || isDirty) && hasError ? "is-invalid" : ""}>
            {choices
             ->Belt.List.mapU((. choice) =>
                 <div key={choice.string} className="form-check">
                   <input
                     id={normalizeId(id ++ "--" ++ choice.string)}
                     className="form-check-input"
                     type_="radio"
                     value={choice.string}
                     checked={string == choice.string}
                     onChange={onChange(choice.value)}
                   />
                   <label htmlFor={normalizeId(id ++ "--" ++ choice.string)} className="form-check-label">
                     {React.string(choice.label)}
                   </label>
                 </div>
               )
             ->Belt.List.toArray
             ->React.array}
          </div>;
        };
      },
      (value, nbSubmits, isDirty, hasError, key, className, choices, expanded, id),
    );
  };
};

module Row = {
  [@react.component]
  let make = (~label, ~labelFor=?, ~className="form-group", ~input, ~wrap, ~field) => {
    let fieldField = React.useMemo1(() => `field(field), [|field|]);
    let {Hook.key} = Hook.useMeta(wrap, fieldField);
    let labelFor =
      switch (labelFor) {
      | Some(id) => id
      | None =>
        let prefixId = Wrap.id(wrap);
        normalizeId(prefixId ++ "-" ++ key);
      };

    <div className>
      <label htmlFor=labelFor> {ReasonReact.string(label)} </label>
      input
      <FieldErrors wrap field=fieldField />
    </div>;
  };
};

module ObjectRow = {
  [@react.component]
  let make = (~label, ~className="form-group", ~input, ~wrap, ~field) => {
    let objField = React.useMemo1(() => `obj(field), [|field|]);
    <div className> <label> {ReasonReact.string(label)} </label> input <FieldErrors wrap field=objField /> </div>;
  };
};

module List = {
  [@react.component]
  let make = (~wrap, ~onAdd, ~onRemove, ~label, ~renderInput, ~field) => {
    let listField = React.useMemo1(() => `list(field), [|field|]);
    let count = Hook.useListCount(wrap, field);
    let rows = React.useMemo2(() => Belt.List.makeByU(count, (. i) => field.getRow(i)), (count, field));

    React.useMemo6(
      () =>
        <div className="form-group">
          <label> {ReasonReact.string(label)} </label>
          <ul>
            {rows
             |> Belt.List.mapWithIndex(_, (i, row) =>
                  <li key={string_of_int(i)}>
                    {renderInput(row, i)}
                    <button
                      className="btn btn-light"
                      onClick={event => {
                        ReactEvent.Synthetic.preventDefault(event);
                        onRemove(i);
                      }}>
                      {ReasonReact.string({js|❌|js})}
                    </button>
                  </li>
                )
             |> Belt.List.toArray
             |> ReasonReact.array}
          </ul>
          <FieldErrors wrap field=listField />
          <button
            className="btn btn-secondary"
            onClick={event => {
              ReactEvent.Synthetic.preventDefault(event);
              onAdd();
            }}>
            {ReasonReact.string("Add +")}
          </button>
        </div>,
      (listField, rows, onAdd, onRemove, label, renderInput),
    );
  };
};

module SubmitButton = {
  [@react.component]
  let make = (~wrap, ~text, ~submittingText) => {
    let formMeta = Hook.useFormMeta(wrap);
    let {Hook.isSubmitting, hasErrors} = formMeta;

    <button type_="submit" className={"btn btn-primary " ++ (hasErrors || isSubmitting ? "disabled" : "")}>
      {React.string(isSubmitting ? submittingText : text)}
    </button>;
  };
};

module ResetButton = {
  [@react.component]
  let make = (~wrap, ~initialForm, ~text) => {
    let formMeta = Hook.useFormMeta(wrap);
    let {Hook.isSubmitting} = formMeta;

    <button
      className={"btn btn-light " ++ (isSubmitting ? "disabled" : "")}
      onClick={event => {
        event->ReactEvent.Synthetic.preventDefault;
        wrap->Wrap.dispatch(_form => initialForm);
      }}>
      {React.string(text)}
    </button>;
  };
};

module Form = {
  [@react.component]
  let make = (~wrap, ~className=?, ~onSubmit, ~render) => {
    <form
      ?className
      onSubmit={event => {
        event->ReactEvent.Synthetic.preventDefault;
        wrap->Wrap.dispatch(Form.startSubmit);
        wrap->Wrap.dispatch(Form.clearSubmitErrors);
        wrap->Wrap.dispatch(form =>
          if (Form.formHasErrors(form)) {
            Form.stopSubmit(form);
          } else {
            onSubmit(form)
            |> Js.Promise.then_(value =>
                 switch (value) {
                 | Belt.Result.Ok(_) =>
                   wrap->Wrap.dispatch(Form.submitSuccess);
                   wrap->Wrap.dispatch(Form.stopSubmit);
                   Js.Promise.resolve();
                 | Error(error) =>
                   wrap->Wrap.dispatch(Form.addSubmitError(error));
                   wrap->Wrap.dispatch(Form.stopSubmit);
                   Js.Promise.resolve();
                 }
               )
            |> ignore;
            form;
          }
        );
      }}>
      render
    </form>;
  };
};
