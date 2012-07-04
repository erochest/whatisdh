
$ ->
  TokenChain  = Backbone.Model.extend()
  TokenChains = Backbone.Paginator.requestPager.extend
    model : TokenChain
    url   : '@{IndexR}'

    skipAttribute    : '$offset'
    perPageAttribute : '$limit'
    sortAttribute    : '$sort'
    orderAttribute   : '$orderBy'

    perPage : 100

    parse : (response) ->
      @totalPages = Math.floor(response.count / @perPage)
      response.results

