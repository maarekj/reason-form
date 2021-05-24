let normalizeId = id => {
  let reg = %re("/[^-_a-zA-Z0-9]/g")
  id |> Js.String.replaceByRe(reg, "-")
}

module FieldErrors = {
  module Pure = {
    @react.component
    let make = React.memo((~nbSubmits, ~isAlreadyBlur, ~errors) =>
      (nbSubmits > 0 || isAlreadyBlur) && Belt.List.length(errors) != 0
        ? <div className="invalid-feedback" style={ReactDOM.Style.make(~display="block", ())}>
            {errors->Belt.List.map(React.string)->Belt.List.toArray->React.array}
          </div>
        : React.null
    )
  }

  @react.component
  let make = (~wrap, ~field) => {
    let {Hook.nbSubmits: nbSubmits} = Hook.useFormMeta(wrap)
    let {Hook.isAlreadyBlur: isAlreadyBlur, errors} = Hook.useMeta(wrap, field)

    <Pure nbSubmits isAlreadyBlur errors />
  }
}

module FormErrors = {
  module Pure = {
    @react.component
    let make = React.memo((
      ~nbSubmits,
      ~hasRootErrors,
      ~hasSubmitErrors,
      ~rootErrors,
      ~submitErrors,
    ) =>
      nbSubmits > 0 && (hasRootErrors || hasSubmitErrors)
        ? <div className="invalid-feedback" style={ReactDOM.Style.make(~display="block", ())}>
            {rootErrors->Belt.List.map(React.string)->Belt.List.toArray->React.array}
            {submitErrors->Belt.List.map(React.string)->Belt.List.toArray->React.array}
          </div>
        : React.null
    )
  }

  @react.component
  let make = (~wrap) => {
    let {
      Hook.nbSubmits: nbSubmits,
      hasRootErrors,
      hasSubmitErrors,
      rootErrors,
      submitErrors,
    } = Hook.useFormMeta(wrap)

    <Pure nbSubmits hasRootErrors hasSubmitErrors rootErrors submitErrors />
  }
}

module Input = {
  module Pure = {
    @react.component
    let make = React.memo((
      ~type_="text",
      ~valueText,
      ~nbSubmits,
      ~isAlreadyBlur,
      ~hasError,
      ~className="form-control",
      ~onFocus,
      ~onBlur,
      ~onChange,
      ~id=?,
    ) => {
      Js.log("render input (#" ++ (id->Belt.Option.getWithDefault("") ++ ")"))

      <input
        type_
        ?id
        className={className ++
        (" " ++
        ((nbSubmits > 0 || isAlreadyBlur) && hasError ? "is-invalid" : ""))}
        value=valueText
        onFocus
        onBlur
        onChange
      />
    })
  }

  @react.component
  let make = (~wrap, ~type_=?, ~id=?, ~className=?, ~toText, ~fromText, ~field) => {
    let value = Hook.useValue(wrap, field)
    let {Hook.nbSubmits: nbSubmits} = Hook.useFormMeta(wrap)
    let {Hook.isAlreadyBlur: isAlreadyBlur, hasError, key} = Hook.useMeta(wrap, field)

    let onFocus = React.useCallback2(_event => wrap->Wrap.dispatch(Form.focus(key)), (wrap, key))
    let onBlur = React.useCallback2(_event => wrap->Wrap.dispatch(Form.blur(key)), (wrap, key))

    let onChange = React.useCallback3(event => {
      let value = fromText(Helper.getValue(event))
      wrap->Wrap.dispatch(form => {
        let value = field.setValue(value, Form.getValues(form))
        Form.changeValues(list{key}, value, form)
      })
    }, (wrap, key, fromText))

    let id = switch id {
    | Some(id) => id
    | None =>
      let prefixId = Wrap.id(wrap)
      normalizeId(prefixId ++ ("-" ++ key))
    }

    <Pure
      ?type_
      valueText={toText(value)}
      nbSubmits
      isAlreadyBlur
      hasError
      ?className
      id
      onFocus
      onBlur
      onChange
    />
  }
}

module Choice = {
  type choice<'v> = {
    value: 'v,
    string: string,
    label: string,
  }

  module Pure = {
    type choice = {
      pureString: string,
      pureLabel: string,
    }

    @react.component
    let make = React.memo((
      ~expanded,
      ~className="form-control",
      ~nbSubmits,
      ~isAlreadyBlur,
      ~hasError,
      ~id,
      ~valueString,
      ~onFocus,
      ~onBlur,
      ~onChange,
      ~onChangeWithString,
      ~choices,
    ) => {
      Js.log("render choice (#" ++ (id ++ ")"))

      switch expanded {
      | false =>
        <select
          id
          className={className ++
          (" " ++
          ((nbSubmits > 0 || isAlreadyBlur) && hasError ? "is-invalid" : ""))}
          value=valueString
          onFocus
          onBlur
          onChange>
          {choices
          ->Belt.List.mapU((. choice) =>
            <option key=choice.pureString value=choice.pureString>
              {React.string(choice.pureLabel)}
            </option>
          )
          ->Belt.List.toArray
          ->React.array}
        </select>
      | true =>
        <div className={(nbSubmits > 0 || isAlreadyBlur) && hasError ? "is-invalid" : ""}>
          {choices
          ->Belt.List.mapU((. choice) =>
            <div key=choice.pureString className="form-check">
              <input
                id={normalizeId(id ++ ("--" ++ choice.pureString))}
                className="form-check-input"
                type_="radio"
                value=choice.pureString
                checked={valueString == choice.pureString}
                onChange={onChangeWithString(choice.pureString)}
              />
              <label
                htmlFor={normalizeId(id ++ ("--" ++ choice.pureString))}
                className="form-check-label">
                {React.string(choice.pureLabel)}
              </label>
            </div>
          )
          ->Belt.List.toArray
          ->React.array}
        </div>
      }
    })
  }

  @react.component
  let make = (~wrap, ~expanded=false, ~className=?, ~id=?, ~choices, ~field) => {
    let value = Hook.useValue(wrap, field)
    let {Hook.nbSubmits: nbSubmits} = Hook.useFormMeta(wrap)
    let {Hook.isAlreadyBlur: isAlreadyBlur, hasError} = Hook.useMeta(wrap, field)

    let toString = React.useCallback1(value => {
      let choice = choices->Belt.List.getByU((. choice) => choice.value == value)
      switch choice {
      | Some({string}) => string
      | None => ""
      }
    }, [choices])

    let fromString = React.useCallback1(string => {
      let choice = choices->Belt.List.getByU((. choice) => choice.string == string)
      switch choice {
      | Some({value}) => Some(value)
      | None => None
      }
    }, [choices])

    let key = field.key
    let onFocus = React.useCallback2(_event => wrap->Wrap.dispatch(Form.focus(key)), (wrap, key))
    let onBlur = React.useCallback2(_event => wrap->Wrap.dispatch(Form.blur(key)), (wrap, key))

    let setValue = React.useCallback1((value, form) => {
      let value = field.setValue(value, Form.getValues(form))
      Form.changeValues(list{field.key}, value, form)
    }, [field])

    let onChange = React.useCallback3(event => {
      let value = fromString(Helper.getValue(event))
      switch value {
      | Some(value) => wrap->Wrap.dispatch(setValue(value))
      | None => ()
      }
    }, (wrap, fromString, setValue))

    let onChangeWithString = React.useCallback3((string, _event) => {
      let value = fromString(string)
      switch value {
      | Some(value) => wrap->Wrap.dispatch(setValue(value))
      | None => ()
      }
    }, (wrap, fromString, setValue))

    let pureChoices = React.useMemo1(
      () => choices->Belt.List.mapU((. c) => {Pure.pureString: c.string, pureLabel: c.label}),
      [choices],
    )

    let id = switch id {
    | Some(id) => id
    | None =>
      let prefixId = Wrap.id(wrap)
      normalizeId(prefixId ++ ("-" ++ field.key))
    }

    let valueString = toString(value)

    <Pure
      expanded
      ?className
      nbSubmits
      isAlreadyBlur
      hasError
      id
      valueString
      onFocus
      onBlur
      onChange
      onChangeWithString
      choices=pureChoices
    />
  }
}

module Row = {
  @react.component
  let make = (
    ~label,
    ~labelFor=?,
    ~className="form-group",
    ~input,
    ~wrap,
    ~field: Field.t<_, _>,
  ) => {
    let key = field.key

    let labelFor = switch labelFor {
    | Some(id) => id
    | None =>
      let prefixId = Wrap.id(wrap)
      normalizeId(prefixId ++ ("-" ++ key))
    }

    <div className>
      <label htmlFor=labelFor> {React.string(label)} </label> input <FieldErrors wrap field />
    </div>
  }
}

module List = {
  module Pure = {
    @react.component
    let make = React.memo((~rows, ~onAdd, ~onRemove, ~label, ~renderInput, ~fieldErrors) =>
      <div className="form-group">
        <label> {React.string(label)} </label>
        <ul>
          {rows
          ->Belt.List.map(i =>
            <li key={string_of_int(i)}>
              {renderInput(i)}
              <button
                className="btn btn-light"
                onClick={event => {
                  ReactEvent.Synthetic.preventDefault(event)
                  onRemove(i)
                }}>
                {React.string(`❌`)}
              </button>
            </li>
          )
          ->Belt.List.toArray
          ->React.array}
        </ul>
        fieldErrors
        <button
          className="btn btn-secondary"
          onClick={event => {
            ReactEvent.Synthetic.preventDefault(event)
            onAdd()
          }}>
          {React.string("Add +")}
        </button>
      </div>
    )
  }

  @react.component
  let make = (~wrap, ~onAdd, ~onRemove, ~label, ~renderInput, ~field) => {
    let count = Hook.useListCount(wrap, field)
    let rows = React.useMemo1(() => Belt.List.makeByU(count, (. i) => i), [count])

    let fieldErrors = React.useMemo2(() => <FieldErrors wrap field />, (wrap, field))

    <Pure rows onAdd onRemove label renderInput fieldErrors />
  }
}

module StringMap = {
  module Pure = {
    @react.component
    let make = React.memo((~allKeys, ~fieldErrors, ~renderInput, ~label) =>
      <div className="form-group">
        <label> {React.string(label)} </label>
        <div>
          {allKeys->Belt.Array.map(key => <div key> {renderInput(key)} </div>)->React.array}
        </div>
        fieldErrors
      </div>
    )
  }

  @react.component
  let make = (~wrap, ~label, ~renderInput, ~field: Field.t<_, _>) => {
    let allKeys = Hook.useStringMapAllKeys(wrap, field)
    let fieldErrors = React.useMemo2(() => <FieldErrors wrap field />, (wrap, field))

    let renderInput = React.useMemo0(() => renderInput)

    <Pure allKeys fieldErrors renderInput label />
  }
}

module SubmitButton = {
  @react.component
  let make = (~wrap, ~text, ~submittingText) => {
    let formMeta = Hook.useFormMeta(wrap)
    let {Hook.isSubmitting: isSubmitting, hasErrors} = formMeta

    <button
      type_="submit"
      className={"btn btn-primary " ++ (hasErrors || isSubmitting ? "disabled" : "")}>
      {React.string(isSubmitting ? submittingText : text)}
    </button>
  }
}

module ResetButton = {
  @react.component
  let make = (~wrap, ~initialForm, ~text) => {
    let formMeta = Hook.useFormMeta(wrap)
    let {Hook.isSubmitting: isSubmitting} = formMeta

    <button
      className={"btn btn-light " ++ (isSubmitting ? "disabled" : "")}
      onClick={event => {
        event->ReactEvent.Synthetic.preventDefault
        wrap->Wrap.dispatch(_form => initialForm)
      }}>
      {React.string(text)}
    </button>
  }
}

module Form = {
  @react.component
  let make = (~wrap, ~className=?, ~onSubmit, ~render) =>
    <form
      ?className
      onSubmit={event => {
        event->ReactEvent.Synthetic.preventDefault
        wrap->Wrap.dispatch(Form.startSubmit)
        wrap->Wrap.dispatch(Form.clearSubmitErrors)
        wrap->Wrap.dispatch(form =>
          if Form.formHasErrors(form) {
            Form.stopSubmit(form)
          } else {
            onSubmit(form)
            |> Js.Promise.then_(value =>
              switch value {
              | Ok(_) =>
                wrap->Wrap.dispatch(Form.submitSuccess)
                wrap->Wrap.dispatch(Form.stopSubmit)
                Js.Promise.resolve()
              | Error(error) =>
                wrap->Wrap.dispatch(Form.addSubmitError(error))
                wrap->Wrap.dispatch(Form.stopSubmit)
                Js.Promise.resolve()
              }
            )
            |> ignore
            form
          }
        )
      }}>
      render
    </form>
}
