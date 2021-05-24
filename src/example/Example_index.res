module App = {
  @react.component
  let make = () => {
    Js.log("App")
    <Example_user_form />
  }
}

switch ReactDOM.querySelector("#app") {
| Some(app) => ReactDOM.render(<App />, app)
| None => failwith("#app not found")
}
