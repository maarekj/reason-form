open ReasonForm;

module Address = Reasonform_example_address_fields;
module User = Reasonform_example_user_fields;

let initialForm = User.initializeForm();

module ReactForm = {
  include React.Make({
    type values = User.Value.t;
    let initialForm = initialForm;
  });
};

module Int = {
  type t = int;
};
module WithOptionStringField = React.WithField(ReactForm, (React.MakeOption(String)));
module WithStringField = React.WithField(ReactForm, String);
module WithIntField = React.WithField(ReactForm, Int);
module WithFieldObject = React.WithFieldObject(ReactForm);
module WithListAddress = React.WithFieldList(ReactForm, (Address.Fields(User.Value)));
module WithListString = React.WithFieldList(ReactForm, (Field.MakeFieldType(User.Value, String)));

module RenderHelper = Reasonform_example_render_helper;
module AddressComponent = Reasonform_example_address_form.Make(ReactForm);

let delay = ms => {
  let unit = ();
  Js.Promise.make((~resolve, ~reject as _) => ignore(Js.Global.setTimeout(() => resolve(. unit), ms)));
};

let reactForm: module React.S = (module ReactForm);

let component = ReasonReact.statelessComponent("UserForm");
let make = _children => {
  ...component,
  render: _ =>
    <ReactForm
      initialForm
      onSubmit={
        (~dispatch as _, ~form) => {
          let values = Form.getValues(form);
          Js.log(values);
          delay(3000)
          |> Js.Promise.then_(() => {
               Js.log("after 3000ms");
               Js.Promise.resolve(Belt.Result.Error("Submit error"));
             });
        }
      }
      render={
        (~dispatch, ~handleSubmit) =>
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
              <WithOptionStringField
                field=User.lastname
                render={
                  withField =>
                    <RenderHelper.Row
                      reactForm
                      withField
                      label={js|Lastname|js}
                      input={
                        <RenderHelper.Input
                          reactForm
                          dispatch
                          toText={Belt.Option.getWithDefault(_, "")}
                          fromText={v => v == "" ? None : Some(v)}
                          withField
                        />
                      }
                    />
                }
              />
              <WithOptionStringField
                field=User.firstname
                render={
                  withField =>
                    <RenderHelper.Row
                      reactForm
                      withField
                      label={js|Firstname|js}
                      input={
                        <RenderHelper.Input
                          reactForm
                          dispatch
                          toText={Belt.Option.getWithDefault(_, "")}
                          fromText={v => v == "" ? None : Some(v)}
                          withField
                        />
                      }
                    />
                }
              />
              <WithOptionStringField
                field=User.username
                render={
                  withField =>
                    <RenderHelper.Row
                      reactForm
                      withField
                      label={js|Username|js}
                      input={
                        <RenderHelper.Input
                          reactForm
                          withField
                          dispatch
                          toText={Belt.Option.getWithDefault(_, "")}
                          fromText={v => v == "" ? None : Some(v)}
                        />
                      }
                    />
                }
              />
              <WithIntField
                field=User.age
                render={
                  withField =>
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
                    />
                }
              />
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
                                  input={
                                    <RenderHelper.Input
                                      reactForm
                                      dispatch
                                      toText={v => v}
                                      fromText={v => v}
                                      withField
                                    />
                                  }
                                />
                            }
                          />
                      }
                    />
                }
              />
              <WithFieldObject
                field=User.mainAddress
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
                render={
                  withField =>
                    <RenderHelper.List
                      reactForm
                      withField
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
          </form>
      }
    />,
};
