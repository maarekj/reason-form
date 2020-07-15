let getValue = event => event->ReactEvent.Form.target##value;

type dispatch('v, 'e) = (Form.form('v, 'e) => Form.form('v, 'e)) => unit;

let handleFormChange =
    (dispatch: dispatch('values, 'e), field: Field.t('values, 'value), mapper: 'b => 'value, param: 'b): unit => {
  let value = mapper(param);
  dispatch(form => {
    let values = Form.getValues(form);
    let newValues = field.setValue(value, values);
    Form.changeValues([field.key], newValues, form);
  });
};

let handleDomFormChange =
    (
      dispatch: dispatch('values, 'e),
      field: Field.t('values, 'value),
      mapper: string => 'value,
      event: ReactEvent.Form.t,
    )
    : unit => {
  let value = mapper(getValue(event));
  dispatch(form => {
    let values = Form.getValues(form);
    let newValues = field.setValue(value, values);
    Form.changeValues([field.key], newValues, form);
  });
};

type field('values, 'v, 'fields, 'row, 'mapFields) = [
  | `field(Field.t('values, 'v))
  | `obj(FieldObject.t('values, 'v, 'fields))
  | `list(FieldList.t('values, 'v, 'row))
  | `map(FieldMap.t('values, 'v, 'mapFields))
];

let key: field(_, _, _, _, _) => string =
  fun
  | `field({Field.key}) => key
  | `obj({FieldObject.key}) => key
  | `list({FieldList.key}) => key
  | `map({FieldMap.key}) => key;

let focus = (field, form) => Form.focus(key(field), form);
let blur = (field, form) => Form.blur(key(field), form);
let clearErrors = (field, form) => Form.clearErrors(key(field), form);
let addError = (field, message, form) => Form.addError(key(field), message, form);
let hasFocus = (field, form) => Form.hasFocus(key(field), form);
let isBlur = (field, form) => Form.isBlur(key(field), form);
let isDirty = (field, form) => Form.isDirty(key(field), form);
let isAlreadyBlur = (field, form) => Form.isAlreadyBlur(key(field), form);

let hasError = (field, form) => Form.hasError(key(field), form);
let getErrors = (field, form) => Form.getErrors(key(field), form);

module List = {
  let setList = (field, list, form) => {
    let list = field.FieldList.setList(list, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };

  let add = (field, value, form) => {
    let list = field.FieldList.add(value, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };

  let push = (field, value, form) => {
    let list = field.FieldList.push(value, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };

  let insert = (field, index, value, form) => {
    let list = field.FieldList.insert(index, value, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };

  let remove = (field, index, form) => {
    let list = field.FieldList.remove(index, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };

  let update = (field, updater, form) => {
    let list = field.FieldList.update(updater, Form.getValues(form));
    Form.changeValues([field.key], list, form);
  };
};
