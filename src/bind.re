type t('values, 'value) = {
  key: string,
  focus: Form.form('values) => Form.form('values),
  blur: Form.form('values) => Form.form('values),
  addError: (string, Form.form('values)) => Form.form('values),
  clearErrors: Form.form('values) => Form.form('values),
  hasFocus: Form.form('values) => bool,
  isBlur: Form.form('values) => bool,
  isDirty: Form.form('values) => bool,
  hasError: Form.form('values) => bool,
  getErrors: Form.form('values) => list(string),
  setValue: ('value, Form.form('values)) => Form.form('values),
  getValue: Form.form('values) => 'value,
};

let bind = (~key, ~getValue, ~setValue) : t('values, 'value) => {
  key,
  focus: form => Form.focus(key, form),
  blur: form => Form.blur(key, form),
  addError: (error, form) => Form.addError(key, error, form),
  clearErrors: form => Form.clearErrors(key, form),
  hasFocus: form => Form.hasFocus(key, form),
  isBlur: form => Form.isBlur(key, form),
  isDirty: form => Form.isDirty(key, form),
  hasError: form => Form.hasError(key, form),
  getErrors: form => Form.getErrors(key, form),
  setValue: (value, form) => {
    let value = setValue(value, Form.getValues(form));
    Form.changeValues([key], value, form);
  },
  getValue: form => getValue(Form.getValues(form)),
};