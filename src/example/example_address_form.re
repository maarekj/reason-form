module Make = (ReactForm: React.S) => {
  module Address = Example_address_fields;
  module RenderHelper = Example_render_helper;
  module WithStringField = React.WithField(ReactForm, String);

  let reactForm: module React.S = (module ReactForm);

  let component = ReasonReact.statelessComponent("UserForm");
  let make = (~dispatch, ~fields: Address.fields(_), ~title=?, _children) => {
    ...component,
    render: _ =>
      <div className="card">
        <div className="card-body">
          {
            switch (title) {
            | Some(title) => <h5 className="card-title"> {ReasonReact.string(title)} </h5>
            | None => ReasonReact.null
            }
          }
          <WithStringField
            field={fields.street}
            render={
              withField =>
                <RenderHelper.Row
                  reactForm
                  withField
                  label={js|Street|js}
                  input={<RenderHelper.Input withField reactForm dispatch toText={v => v} fromText={v => v} />}
                />
            }
          />
          <WithStringField
            field={fields.city}
            render={
              withField =>
                <RenderHelper.Row
                  reactForm
                  withField
                  label={js|City|js}
                  input={<RenderHelper.Input reactForm dispatch toText={v => v} fromText={v => v} withField />}
                />
            }
          />
        </div>
      </div>,
  };
};
