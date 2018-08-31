let getValue = event => ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value;

type dispatch('a) = (Form.form('a) => Form.form('a)) => unit;

let handleDomFormChange =
    (dispatch: dispatch('a), field: Field.t('values, 'value), mapper: string => 'value, event: ReactEventRe.Form.t)
    : unit => {
  let value = mapper(getValue(event));
  dispatch(form => {
    let values = Form.getValues(form);
    let newValues = field.setValue(value, values);
    Form.changeValues([field.key], newValues, form);
  });
};
