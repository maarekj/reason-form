type t('values, 'obj, 'fields) = {
  key: string,
  getObject: 'values => 'obj,
  setObject: ('obj, 'values) => 'values,
  fields: 'fields,
};

let createField =
    (~key: string, ~getObject: 'values => 'obj, ~setObject: ('obj, 'values) => 'values, ~createFields)
    : t('values, 'obj, 'fields) => {
  key,
  getObject,
  setObject,
  fields:
    createFields({
      Field.wrapField: (field: Field.t(_, _)) =>
        Field.wrapField(~key=key ++ "." ++ field.key, ~field, ~getValue=getObject, ~setValue=setObject, ()),
    }),
};

let changeValues = (field, newValues, form) => Form.changeValues([field.key], newValues, form);
