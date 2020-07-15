module StringMap = Belt.Map.String;

type t('values, 'value, 'fields) = {
  key: string,
  allKeys: 'values => list(string),
  countKeys: 'values => int,
  set: (string, 'value, 'values) => 'values,
  get: (string, 'values) => option('value),
  has: (string, 'values) => bool,
  remove: (string, 'values) => 'values,
  update: (StringMap.t('value) => StringMap.t('value), 'values) => 'values,
  getMap: 'values => StringMap.t('value),
  setMap: (StringMap.t('value), 'values) => 'values,
  getFields: string => 'fields,
};

let createField =
    (
      ~key: string,
      ~getMap: 'values => StringMap.t('value),
      ~setMap: (StringMap.t('value), 'values) => 'values,
      ~createFields: Field.wrapper('value, 'values, 'fields) => 'mapFields,
    )
    : t('values, 'value, 'mapFields) => {
  {
    key,
    allKeys: values => values->getMap->StringMap.keysToArray->Belt.List.fromArray,
    countKeys: values => values->getMap->StringMap.keysToArray->Belt.Array.length,
    set: (key, value, values) => values->getMap->StringMap.set(key, value)->setMap(values),
    get: (key, values) => values->getMap->StringMap.get(key),
    has: (key, values) => values->getMap->StringMap.has(key),
    getMap,
    setMap,
    remove: (key, values) => values->getMap->StringMap.remove(key)->setMap(values),
    update: (updater, values) => updater(values->getMap)->setMap(values),
    getFields: mapKey => {
      createFields({
        Field.wrapField: (field: Field.t(_, _)) =>
          Field.wrapField(
            ~key=key ++ "[" ++ mapKey ++ "]." ++ field.key,
            ~field,
            ~getValue=values => values->getMap->StringMap.getExn(mapKey),
            ~setValue=(value, values) => values->getMap->StringMap.set(mapKey, value)->setMap(values),
            (),
          ),
      });
    },
  };
};
