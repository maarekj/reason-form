module Render = BootstrapRender

@react.component
let make = (~wrap, ~fields: Example_metadata_fields.fields<_>, ~title=?) =>
  <div className="card">
    <div className="card-body">
      {switch title {
      | Some(title) => <h5 className="card-title"> {React.string(title)} </h5>
      | None => React.null
      }}
      <div className="row">
        <div className="col-sm-8">
          <Render.Row
            label=`Meta-title`
            wrap
            field=fields.title
            input={<Render.Input
              wrap type_="text" field=fields.title toText={v => v} fromText={v => v}
            />}
          />
        </div>
        <div className="col-sm-4">
          <Render.Row
            label=`Meta-Description`
            wrap
            field=fields.desc
            input={<Render.Input
              wrap type_="text" field=fields.desc toText={v => v} fromText={v => v}
            />}
          />
        </div>
      </div>
    </div>
  </div>
