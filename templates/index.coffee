
$ ->
  TokenChain  = Backbone.Model.extend()
  TokenChains = Backbone.Paginator.requestPager.extend
    model : TokenChain
    url   : '@{IndexDataR}?&'

    skipAttribute    : '$offset'
    perPageAttribute : '$limit'
    sortAttribute    : '$sort'
    orderAttribute   : '$orderBy'

    page          : 0
    firstPage     : 0
    perPage       : 100
    sortField     : 'token_1'
    sortDirection : 'asc'
    format        : 'json'

    parse : (response) ->
      @totalPages = Math.floor(response.count / @perPage)
      response.results

