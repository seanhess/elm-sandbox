
Proposal: 
========

Motivation: Elm architecture is nice and clean, but it gets complicated when you add effects. For example:

MainView.elm

    import SearchView as Search

    type State = {
      search : Search.State
      result : String
    }

    init = { search = Search.init, result = "" }

SearchView.elm

    type State = {
      searchTerm : String
    }

    type Action
      | Type String
      = Loaded (Result HTTP.Error String)

    update : Action -> State -> (State, Effects Action)
    update action state =
      case action of


