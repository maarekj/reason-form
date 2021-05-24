module Value = {
  type t = {
    title: string,
    desc: string,
  }

  let empty: t = {title: "", desc: ""}
}

type fields<'t, 'self> = {
  self: Field.t<'t, 'self>,
  title: Field.t<'t, string>,
  desc: Field.t<'t, string>,
}

let createFields = (self, baseField) => {
  open Field
  {
    self: self,
    title: \"+|>"(
      baseField,
      createField(
        ~key="title",
        ~getValue=(v: Value.t) => v.title,
        ~setValue=(title, v) => {...v, title: title},
      ),
    ),
    desc: \"+|>"(
      baseField,
      createField(
        ~key="desc",
        ~getValue=(v: Value.t) => v.desc,
        ~setValue=(desc, v) => {...v, desc: desc},
      ),
    ),
  }
}

let validate = (fields: fields<_, Value.t>, form) => {
  let metadata = form->Form.getValues->fields.self.getValue

  let addError = f => Form.addError(f.Field.key)
  let id = form => form
  form
  |> switch metadata.title {
  | "" => addError(fields.title, "Title is required.")
  | "forbidden" => addError(fields.title, "Forbidden title.")
  | _ => id
  }
  |> switch metadata.desc {
  | "forbidden" => addError(fields.desc, "Forbidden desc.")
  | _ => id
  }
}
