module Address = Example_address_fields;
module User = Example_user_fields;

module Make = (ReactForm: React.S with type values = User.Value.t) => {
  module Int = {
    type t = int;
  };
  module WithOptionStringField = React.WithField(ReactForm, (React.MakeOption(String)));
  module WithStringField = React.WithField(ReactForm, String);
  module WithIntField = React.WithField(ReactForm, Int);
  module WithFieldObject = React.WithFieldObject(ReactForm);
  module WithListAddress = React.WithFieldList(ReactForm, (Address.Fields(User.Value)));
  module WithListString = React.WithFieldList(ReactForm, (Field.MakeFieldType(User.Value, String)));

  module RenderHelper = Example_render_helper;
  module AddressComponent = Example_address_form.Make(ReactForm);

  let reactForm: module React.S = (module ReactForm);

  let renderStringField = (label, dispatch, withField) =>
    <RenderHelper.Row
      reactForm
      withField
      label
      input={
        <div>
          <RenderHelper.Input
            reactForm
            dispatch
            toText={Belt.Option.getWithDefault(_, "")}
            fromText={v => v == "" ? None : Some(v)}
            withField
          />
        </div>
      }
    />;

  let renderLastname = (state, dispatch, withField) =>
    <> <p> {ReasonReact.string(string_of_int(state))} </p> {renderStringField("Lastname", dispatch, withField)} </>;

  let renderFirstname = renderStringField("Firstname");
  let renderUsername = renderStringField("Username");
  let renderAge = (dispatch, withField) =>
    <RenderHelper.Row
      reactForm
      withField
      label={js|Age|js}
      input={
        <RenderHelper.Input
          reactForm
          withField
          dispatch
          toText=string_of_int
          fromText={
            v =>
              try (int_of_string(v)) {
              | Failure("int_of_string") => 0
              }
          }
        />
      }
    />;

  let component = ReasonReact.statelessComponent("UserRenderForm");
  let make = (~state, ~initialForm, ~dispatch, ~handleSubmit, _children) => {
    ...component,
    render: _ => {
      Js.log("form render");
      let renderLastname = renderLastname(state, dispatch);
      let renderFirstname = renderFirstname(dispatch);
      let renderUsername = renderUsername(dispatch);
      let renderAge = renderAge(dispatch);
      <form
        className="card"
        onSubmit={
          event => {
            event->ReactEvent.Synthetic.preventDefault;
            handleSubmit();
          }
        }
        style={
          ReactDOMRe.Style.make(
            ~width="600px",
            ~marginTop="60px",
            ~marginBottom="60px",
            ~marginLeft="auto",
            ~marginRight="auto",
            (),
          )
        }>
        <div className="card-body">
          <p> {ReasonReact.string(string_of_int(state))} </p>
          <WithOptionStringField field=User.lastname pure=false render=renderLastname />
          <WithOptionStringField field=User.firstname pure=true render=renderFirstname />
          <WithOptionStringField field=User.username pure=true render=renderUsername />
          <WithIntField field=User.age pure=true render=renderAge />
          <WithListString
            field=User.tags
            render={
              withField =>
                <RenderHelper.List
                  reactForm
                  withField
                  label={js|Tags|js}
                  onAdd={({actions: {push}}) => dispatch(push(""))}
                  onRemove={({actions: {remove}}, i) => dispatch(remove(i))}
                  renderInput={
                    (~row, ~index) =>
                      <WithStringField
                        field=row
                        render={
                          withField =>
                            <RenderHelper.Row
                              reactForm
                              withField
                              label={j|Tag $(index)|j}
                              input=
                                <>
                                  <p> {ReasonReact.string(string_of_int(state))} </p>
                                  <RenderHelper.Input reactForm dispatch toText={v => v} fromText={v => v} withField />
                                </>
                            />
                        }
                      />
                  }
                />
            }
          />
          <WithFieldObject
            field=User.mainAddress
            pure=true
            render={
              withField =>
                <RenderHelper.Obj
                  reactForm
                  withField
                  label={js|Main address|js}
                  input={<AddressComponent fields={withField.fields} dispatch />}
                />
            }
          />
          <WithListAddress
            field=User.addresses
            pure=true
            render={
              withField =>
                <RenderHelper.List
                  reactForm
                  withField
                  pure=true
                  label={js|Addresses|js}
                  onAdd={({actions: {push}}) => dispatch(push(Address.Value.empty))}
                  onRemove={({actions: {remove}}, i) => dispatch(remove(i))}
                  renderInput={
                    (~row, ~index) =>
                      <AddressComponent title={"Address " ++ string_of_int(index)} fields=row dispatch />
                  }
                />
            }
          />
          <RenderHelper.FormErrors reactForm />
        </div>
        <div className="card-footer">
          <RenderHelper.SubmitButton reactForm text="Submit" submittingText="Pending..." />
          <RenderHelper.ResetButton reactForm dispatch initialForm text="Reset" />
        </div>
      </form>;
    },
  };
};
