type t('values, 'obj, 'fields) = {
  key: string,
  getObject: 'values => 'obj,
  setObject: ('obj, 'values) => 'values,
  fields: 'fields,
  bind: Bind.t('values, 'obj),
};

type wrapper('obj, 'values, 'fields) = {wrapField: 'a. Field.t('obj, 'a) => Field.t('values, 'a)};

let createField =
    (~key: string, ~getObject: 'values => 'obj, ~setObject: ('obj, 'values) => 'values, ~createFields)
    : t('values, 'obj, 'fields) => {
  key,
  getObject,
  setObject,
  bind: Bind.bind(~key, ~getValue=getObject, ~setValue=setObject),
  fields:
    createFields({
      wrapField: (field: Field.t(_, _)) =>
        Field.wrapField(~key=key ++ "." ++ field.key, ~field, ~getValue=getObject, ~setValue=setObject, ()),
    }),
};

let changeValues = (field, newValues, form) => Form.changeValues([field.key], newValues, form);
