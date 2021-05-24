module Render = BootstrapRender
module Fields = Example_user_fields
module Address = Example_address_fields
module AddressForm = Example_address_form
module MetadataForm = Example_metadata_form

let toText = a => a->Belt.Option.getWithDefault("")
let fromText = a => Some(a)

module GenderRow = {
  @react.component
  let make = (~wrap, ~field: Field.t<_, option<Fields.Gender.t>>, ~expanded) => {
    let value = Hook.useValue(wrap, field)

    React.useMemo3(() => {
      let icon = switch value {
      | None => React.null
      | Some(Male) => <span className="input-group-text"> <i className="fas fa-mars" /> </span>
      | Some(Female) => <span className="input-group-text"> <i className="fas fa-venus" /> </span>
      | Some(Other) =>
        <span className="input-group-text"> <i className="fas fa-genderless" /> </span>
      }

      <Render.Row
        label=`Gender`
        wrap
        field
        input={<div className="input-group mb-3">
          <Render.Choice wrap expanded field choices=Fields.Gender.optChoices />
          <div className="input-group-append"> icon </div>
        </div>}
      />
    }, (wrap, expanded, value))
  }
}

@react.component
let make = () => {
  let (expanded, setExpanded) = React.useState(() => false)
  let (wrap, fields) = Fields.useForm(Fields.Value.empty)

  let {Hook.isSubmitting: isSubmitting} = Hook.useFormMeta(wrap)

  React.useMemo2(() =>
    <Render.Form
      wrap
      className="form"
      onSubmit={form => {
        Js.log2("values", Form.getValues(form))
        Js.Promise.make((~resolve, ~reject as _) =>
          Js.Global.setTimeout(() => resolve(. Belt.Result.Error("Error on form")), 3000)->ignore
        )
      }}
      render={<fieldset disabled=isSubmitting>
        <div
          className="card"
          style={ReactDOM.Style.make(
            ~width="600px",
            ~marginLeft="auto",
            ~marginRight="auto",
            ~marginTop="50px",
            (),
          )}>
          <div className="card-body">
            <h1 className="card-title"> {React.string("Hello World")} </h1>
            <Render.Row
              label=`Username`
              wrap
              field=fields.username
              input={<Render.Input wrap type_="text" field=fields.username toText fromText />}
            />
            <div className="row">
              <div className="col-sm-6">
                <Render.Row
                  label=`Lastname`
                  wrap
                  field=fields.lastname
                  input={<Render.Input wrap type_="text" field=fields.lastname toText fromText />}
                />
              </div>
              <div className="col-sm-6">
                <Render.Row
                  label=`Firstname`
                  wrap
                  field=fields.firstname
                  input={<Render.Input wrap type_="text" field=fields.firstname toText fromText />}
                />
              </div>
            </div>
            <GenderRow wrap expanded field=fields.gender />
            <Render.Row
              label=`Age`
              wrap
              field=fields.age
              input={<Render.Input
                wrap
                field=fields.age
                type_="text"
                toText=string_of_int
                fromText={v =>
                  try int_of_string(v) catch {
                  | _ => 0
                  }}
              />}
            />
            <Render.StringMap
              wrap
              field={fields.metadata->fst}
              label=`Méta-données`
              renderInput={key =>
                <MetadataForm wrap fields={fields.metadata->snd(key)} title=key />}
            />
            <Render.List
              wrap
              field={fields.tags->fst}
              label=`Tags`
              onAdd={() => wrap->Wrap.dispatch(Helper.List.push(fields.tags->fst, ""))}
              onRemove={i => wrap->Wrap.dispatch(Helper.List.remove(fields.tags->fst, i))}
              renderInput={index => {
                let field = fields.tags->snd(index)
                <Render.Row
                  wrap
                  field
                  label=j`Tag $(index)`
                  input={<Render.Input wrap toText={v => v} fromText={v => v} field />}
                />
              }}
            />
            <Render.Row
              wrap
              field=fields.mainAddress.self
              label=``
              input={<AddressForm wrap title=`Main Address` fields=fields.mainAddress />}
            />
            <Render.List
              wrap
              field={fields.addresses->fst}
              label=`Addresses`
              onAdd={() =>
                wrap->Wrap.dispatch(Helper.List.push(fields.addresses->fst, Address.Value.empty))}
              onRemove={i => wrap->Wrap.dispatch(Helper.List.remove(fields.addresses->fst, i))}
              renderInput={index =>
                <AddressForm
                  wrap title=j`Address $(index)` fields={fields.addresses->snd(index)}
                />}
            />
            <Render.FormErrors wrap />
          </div>
          <div className="card-footer">
            <Render.SubmitButton wrap text="Submit" submittingText="Submitting..." />
            <Render.ResetButton wrap initialForm=wrap.initial text="Reset" />
            <button
              className="btn btn-xs btn-default"
              onClick={event => {
                ReactEvent.Synthetic.preventDefault(event)
                setExpanded(not)
              }}>
              {React.string("Expanded = " ++ (expanded ? "true" : "false"))}
            </button>
          </div>
        </div>
      </fieldset>}
    />
  , (isSubmitting, expanded))
}
