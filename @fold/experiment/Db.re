type value = Logic.value;

type sMap = Logic.sMap;

module T = {
  module Record = (Item: {}) => {
    include Item;
  };
};

module Message =
  T.Record({
    let author = Person.t;
    let content = T.string;
  });

module Thread = {
  let messages = T.set(Message.t);
};
