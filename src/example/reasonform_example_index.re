let component = ReasonReact.statelessComponent("App");
let make = _children => {...component, render: _ => <Reasonform_example_user_form />};

ReactDOMRe.renderToElementWithId(ReasonReact.element(make([||])), "app");
