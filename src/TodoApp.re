type item = {
  id: int,
  title: string,
  completed: bool
};

let str = ReasonReact.stringToElement;

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, ~deleteTodo, _children) => {
    ...component,
    render: (_) =>
      <li
        className=(item.completed ? "completed" : "")
        onClick=(_evt => onToggle())>
        <div className="view">
          <input
            _type="checkbox"
            className="toggle"
            checked=(Js.Boolean.to_js_boolean(item.completed))
          />
          <label> (str(item.title)) </label>
          <button className="destroy" onClick=(_evt => deleteTodo()) />
        </div>
      </li>
  };
};

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) =>
      <input
        value=text
        className="new-todo"
        _type="text"
        placeholder="Write something to do"
        onChange=(reduce(evt => valueFromEvent(evt)))
        onKeyDown=(
          evt =>
            if (ReactEventRe.Keyboard.key(evt) == "Enter") {
              onSubmit(text);
              (reduce(() => ""))();
            }
        )
      />
  };
};

type state = {items: list(item)};

type action =
  | AddItem(string)
  | ToggleItem(int)
  | DeleteItem(int);

let component = ReasonReact.reducerComponent("TodoApp");

let lastId = ref(2);

let newItem = text => {
  lastId := lastId^ + 1;
  {id: lastId^, title: text, completed: false};
};

let rec len = (myList: list(item)) =>
  switch myList {
  | [] => 0
  | [_, ...tail] => 1 + len(tail)
  };

let getLeftTodosAmount = (items: list(item)) : string => {
  let todosLeft = List.filter(item => ! item.completed == true, items);
  string_of_int(len(todosLeft));
};

let make = _children => {
  ...component,
  initialState: () => {
    items: [
      {id: 0, title: "Reason todoApp", completed: true},
      {id: 1, title: "Write some things to do", completed: false}
    ]
  },
  reducer: (action, {items}) =>
    switch action {
    | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) =>
      let items =
        List.map(
          item =>
            item.id === id ? {...item, completed: ! item.completed} : item,
          items
        );
      ReasonReact.Update({items: items});
    | DeleteItem(id) =>
      let items = List.filter(item => item.id !== id, items);
      ReasonReact.Update({items: items});
    },
  render: ({state: {items}, reduce}) =>
    <section className="todoapp">
      <header className="header">
        <h1> (str("Todos")) </h1>
        <Input onSubmit=(reduce(text => AddItem(text))) />
      </header>
      <section className="main">
        <ul className="todo-list">
          (
            ReasonReact.arrayToElement(
              Array.of_list(
                List.map(
                  item =>
                    <TodoItem
                      key=(string_of_int(item.id))
                      onToggle=(reduce(() => ToggleItem(item.id)))
                      deleteTodo=(reduce(() => DeleteItem(item.id)))
                      item
                    />,
                  items
                )
              )
            )
          )
        </ul>
      </section>
      <footer className="footer">
        <span className="todo-count">
          <strong> (str(getLeftTodosAmount(items))) </strong>
          (str(" item left"))
        </span>
      </footer>
    </section>
};