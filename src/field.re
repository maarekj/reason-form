type t('values, 'value) = {
  key: string,
  setValue: ('value, 'values) => 'values,
  getValue: 'values => 'value,
  bind: Bind.t('values, 'value),
};
type field('values, 'value) = t('values, 'value);

let createField =
    (~key: string, ~getValue: 'values => 'value, ~setValue: ('value, 'values) => 'values)
    : t('values, 'value) => {
  key,
  setValue,
  getValue,
  bind: Bind.bind(~key, ~getValue, ~setValue),
};

let mapField = (~field, ~key: string=field.key, ~getValue, ~setValue, ()) => {
  key,
  getValue,
  setValue,
  bind: Bind.bind(~key=field.key, ~getValue, ~setValue),
};

let wrapField =
    (
      ~field: t('address, 'string),
      ~key: string=field.key,
      ~getValue: 'user => 'address,
      ~setValue: ('address, 'user) => 'user,
      (),
    )
    : t('user, 'string) =>
  mapField(
    ~field,
    ~key,
    ~getValue=
      user => {
        let address = getValue(user);
        field.getValue(address);
      },
    ~setValue=
      (string, user) => {
        let address = getValue(user);
        let newAddress = field.setValue(string, address);
        setValue(newAddress, user);
      },
    (),
  );

let wrapOptionField =
    (~field: t('address, 'string), ~empty: 'address, ~key: string=field.key, ())
    : t(option('address), 'string) =>
  mapField(
    ~field,
    ~key,
    ~getValue=
      address =>
        switch (address) {
        | None => field.getValue(empty)
        | Some(address) => field.getValue(address)
        },
    ~setValue=
      (string, address) =>
        switch (address) {
        | None => Some(field.setValue(string, empty))
        | Some(address) => Some(field.setValue(string, address))
        },
    (),
  );
