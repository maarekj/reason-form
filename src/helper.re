let getValue = event => event->ReactEvent.Form.target##value;

type dispatch('a) = (Form.form('a) => Form.form('a)) => unit;

let handleFormChange =
    (dispatch: dispatch('a), field: Field.t('values, 'value), mapper: 'a => 'value, param: 'a): unit => {
  let value = mapper(param);
  dispatch(form => {
    let values = Form.getValues(form);
    let newValues = field.setValue(value, values);
    Form.changeValues([field.key], newValues, form);
  });
};

let handleDomFormChange =
    (dispatch: dispatch('a), field: Field.t('values, 'value), mapper: string => 'value, event: ReactEvent.Form.t)
    : unit => {
  let value = mapper(getValue(event));
  dispatch(form => {
    let values = Form.getValues(form);
    let newValues = field.setValue(value, values);
    Form.changeValues([field.key], newValues, form);
  });
};
