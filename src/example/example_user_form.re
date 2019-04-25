module Render = BootstrapRender;
module Fields = Example_user_fields;
module Address = Example_address_fields;
module AddressForm = Example_address_form;
let initialForm = Example_user_fields.initializeForm();

let toText = a => a->Belt.Option.getWithDefault("");
let fromText = a => Some(a);

module GenderRow = {
  [@react.component]
  let make = (~wrap, ~expanded) => {
    let value = Hook.useValue(wrap, Fields.gender);

    React.useMemo3(
      () => {
        let icon =
          switch (value) {
          | None => React.null
          | Some(Male) => <span className="input-group-text"> <i className="fas fa-mars" /> </span>
          | Some(Female) => <span className="input-group-text"> <i className="fas fa-venus" /> </span>
          | Some(Other) => <span className="input-group-text"> <i className="fas fa-genderless" /> </span>
          };

        <Render.Row
          label={js|Gender|js}
          wrap
          field=Fields.gender
          input={
            <div className="input-group mb-3">
              <Render.Choice wrap expanded field=Fields.gender choices=Fields.Gender.optChoices />
              <div className="input-group-append"> icon </div>
            </div>
          }
        />;
      },
      (wrap, expanded, value),
    );
  };
};

[@react.component]
let make = () => {
  let (expanded, setExpanded) = React.useState(() => false);
  let wrap = React.useMemo0(() => Wrap.make(initialForm));

  let {Hook.isSubmitting} = Hook.useFormMeta(wrap);

  React.useMemo2(
    () =>
      <Render.Form
        wrap
        className="form"
        onSubmit={form => {
          Js.log2("values", Form.getValues(form));
          Js.Promise.make((~resolve, ~reject as _) =>
            Js.Global.setTimeout(() => resolve(. Belt.Result.Error("Error on form")), 3000)->ignore
          );
        }}
        render={
          <fieldset disabled=isSubmitting>
            <div
              className="card"
              style={ReactDOMRe.Style.make(
                ~width="600px",
                ~marginLeft="auto",
                ~marginRight="auto",
                ~marginTop="50px",
                (),
              )}>
              <div className="card-body">
                <h1 className="card-title"> {React.string("Hello World")} </h1>
                <Render.Row
                  label={js|Username|js}
                  wrap
                  field=Fields.username
                  input={<Render.Input wrap type_="text" field=Fields.username toText fromText />}
                />
                <div className="row">
                  <div className="col-sm-6">
                    <Render.Row
                      label={js|Lastname|js}
                      wrap
                      field=Fields.lastname
                      input={<Render.Input wrap type_="text" field=Fields.lastname toText fromText />}
                    />
                  </div>
                  <div className="col-sm-6">
                    <Render.Row
                      label={js|Firstname|js}
                      wrap
                      field=Fields.firstname
                      input={<Render.Input wrap type_="text" field=Fields.firstname toText fromText />}
                    />
                  </div>
                </div>
                <GenderRow wrap expanded />
                <Render.Row
                  label={js|Age|js}
                  wrap
                  field=Fields.age
                  input={
                    <Render.Input
                      wrap
                      field=Fields.age
                      type_="text"
                      toText=string_of_int
                      fromText={v =>
                        try (int_of_string(v)) {
                        | Failure("int_of_string") => 0
                        }
                      }
                    />
                  }
                />
                <Render.List
                  wrap
                  field=Fields.tags
                  label={js|Tags|js}
                  onAdd={() => wrap->Wrap.dispatch(Helper.List.push(Fields.tags, ""))}
                  onRemove={i => wrap->Wrap.dispatch(Helper.List.remove(Fields.tags, i))}
                  renderInput={(row, index) =>
                    <Render.Row
                      wrap
                      field=row
                      label={j|Tag $(index)|j}
                      input={<Render.Input wrap toText={v => v} fromText={v => v} field=row />}
                    />
                  }
                />
                <Render.ObjectRow
                  wrap
                  field=Fields.mainAddress
                  label={js||js}
                  input={<AddressForm wrap title={js|Main Address|js} fields={Fields.mainAddress.fields} />}
                />
                <Render.List
                  wrap
                  field=Fields.addresses
                  label={js|Addresses|js}
                  onAdd={() => wrap->Wrap.dispatch(Helper.List.push(Fields.addresses, Address.Value.empty))}
                  onRemove={i => wrap->Wrap.dispatch(Helper.List.remove(Fields.addresses, i))}
                  renderInput={(row, index) => <AddressForm wrap title={j|Address $(index)|j} fields=row />}
                />
                <Render.FormErrors wrap />
              </div>
              <div className="card-footer">
                <Render.SubmitButton wrap text="Submit" submittingText="Submitting..." />
                <Render.ResetButton wrap initialForm text="Reset" />
                <button
                  className="btn btn-xs btn-default"
                  onClick={event => {
                    ReactEvent.Synthetic.preventDefault(event);
                    setExpanded((!));
                  }}>
                  {React.string("Expanded = " ++ (expanded ? "true" : "false"))}
                </button>
              </div>
            </div>
          </fieldset>
        }
      />,
    (isSubmitting, expanded),
  );
};
