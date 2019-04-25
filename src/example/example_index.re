module App = {
  [@react.component]
  let make = () => {
    Js.log("App");
    <Example_user_form />;
  };
};

ReactDOMRe.renderToElementWithId(<App />, "app");
