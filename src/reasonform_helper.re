module Form = Reasonform_form;
module Field = Reasonform_field;
module FieldObject = Reasonform_fieldObject;
module FieldList = Reasonform_fieldList;

let getValue = event => event->ReactEvent.Form.target##value;

type dispatch('a) = (Form.form('a) => Form.form('a)) => unit;

let handleFormChange =
    (dispatch: dispatch('a), field: Field.t('values, 'value), mapper: 'b => 'value, param: 'b): unit => {
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

type field('values, 'v, 'fields, 'row) = [
  | `field(Field.t('values, 'v))
  | `obj(FieldObject.t('values, 'v, 'fields))
  | `list(FieldList.t('values, 'v, 'row))
];

let key: field(_, _, _, _) => string =
  fun
  | `field({Field.key}) => key
  | `obj({FieldObject.key}) => key
  | `list({FieldList.key}) => key;

let focus = (field, form) => Form.focus(key(field), form);
let blur = (field, form) => Form.blur(key(field), form);
let clearErrors = (field, form) => Form.clearErrors(key(field), form);
let addError = (field, message, form) => Form.addError(key(field), message, form);
let hasFocus = (field, form) => Form.hasFocus(key(field), form);
let isBlur = (field, form) => Form.isBlur(key(field), form);
let isDirty = (field, form) => Form.isDirty(key(field), form);

let hasError = (field, form) => Form.hasError(key(field), form);
let getErrors = (field, form) => Form.getErrors(key(field), form);
