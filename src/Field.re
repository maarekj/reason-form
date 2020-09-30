type t('values, 'value) = {
  key: string,
  setValue: ('value, 'values) => 'values,
  getValue: 'values => 'value,
};

type field('values, 'value) = t('values, 'value);

let createField =
    (~key: string, ~getValue: 'values => 'value, ~setValue: ('value, 'values) => 'values): t('values, 'value) => {
  key,
  setValue,
  getValue,
};

let idField = key => createField(~key, ~getValue=v => v, ~setValue=(v, _) => v);

let chain = (fa: t(_, _), fb: t(_, _)) => {
  key: fa.key ++ "." ++ fb.key,
  getValue: values => values->(fa.getValue)->(fb.getValue),
  setValue: (v: _, values: _) => {
    let address = values->(fa.getValue);
    let newAddress = fb.setValue(v, address);
    fa.setValue(newAddress, values);
  },
};
let (+|>) = chain;

let option = (field: t(_, _), emptyValue) => {
  key: field.key,
  getValue: values => {
    field.getValue(values)->Belt.Option.getWithDefault(emptyValue);
  },
  setValue: (v, values) => {
    field.setValue(Some(v), values);
  },
};
let (+?|>) = option;

let makeListItemField = (default, field, i: int) => {
  {
    key: field.key ++ "[" ++ i->string_of_int ++ "]",
    getValue: values => {
      field.getValue(values)->Belt.List.get(i)->Belt.Option.getWithDefault(default);
    },
    setValue: (v, values) => {
      let list = values->(field.getValue);

      // If the i is out of bound, the new element will be discarded of the list.
      let newList = list->Belt.List.mapWithIndex((index, e) => i == index ? v : e);

      field.setValue(newList, values);
    },
  };
};

let chainList = (baseField, default, createItemField) => {
  (
    baseField,
    i => {
      let itemField = makeListItemField(default, baseField, i);
      createItemField(itemField, itemField);
    },
  );
};

let makeStringMapItemField = (default, field, key) => {
  {
    key: field.key ++ "[" ++ key ++ "]",
    getValue: values => field.getValue(values)->Belt.Map.String.getWithDefault(key, default),
    setValue: (v, values) => {
      let map = values->(field.getValue);
      let newMap = map->Belt.Map.String.set(key, v);
      field.setValue(newMap, values);
    },
  };
};

let chainStringMap = (baseField, default, createItemField) => {
  (
    baseField,
    key => {
      let itemField = makeStringMapItemField(default, baseField, key);
      createItemField(itemField, itemField);
    },
  );
};
