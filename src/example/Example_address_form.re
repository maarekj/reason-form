module Render = BootstrapRender;

let optionToText = a => a->Belt.Option.getWithDefault("");
let optionFromText = a => Some(a);

[@react.component]
let make = (~wrap, ~fields: Example_address_fields.fields(_), ~title=?) => {
  <div className="card">
    <div className="card-body">
      {switch (title) {
       | Some(title) => <h5 className="card-title"> {React.string(title)} </h5>
       | None => React.null
       }}
      <div className="row">
        <div className="col-sm-12">
          <Render.Row
            label={js|Street|js}
            wrap
            field={fields.street}
            input={<Render.Input wrap type_="text" field={fields.street} toText={v => v} fromText={v => v} />}
          />
        </div>
      </div>
      <div className="row">
        <div className="col-sm-6">
          <Render.Row
            label={js|City|js}
            wrap
            field={fields.city}
            input={<Render.Input wrap type_="text" field={fields.city} toText={v => v} fromText={v => v} />}
          />
        </div>
        <div className="col-sm-6">
          <Render.Row
            label={js|Zipcode|js}
            wrap
            field={fields.zipcode}
            input={
              <Render.Input wrap type_="text" field={fields.zipcode} toText=optionToText fromText=optionFromText />
            }
          />
        </div>
      </div>
    </div>
  </div>;
};
