
$ ->
  # Stub it out
  window.app = 
    collections : {}
    models      : {}
    views       : {}
    mixins      : {}

  # Models/Collections
  window.app.models.Trigram  = Backbone.Model.extend({})
  window.app.collections.TrigramCollection = Backbone.Paginator.requestPager.extend
    model : window.app.models.Trigram

    paginator_core:
      type     : 'GET'
      url      : '@{IndexDataR}'
      dataType : 'json'

    paginator_ui:
      currentPage : 0
      firstPage   : 0
      perPage     : 100
      totalPages  : 100

    server_api:
      'limit'   : () -> @perPage
      'offset'  : () -> @currentPage * @perPage
      'orderby' : 'freq'
      'sort'    : 'desc'
      'format'  : 'json'

    parse : (response) ->
      @totalRecords = parseInt(response.count)
      @totalPages   = Math.floor(response.count / @perPage)
      response.results

  # Views
  window.app.views.ResultView = Backbone.View.extend
    tagName  : 'li'
    template : _.template $('#trigramTemplate').html()

    initialize: () ->
      @model.bind 'change',  this.render, this
      @model.bind 'destroy', this.remove, this

    render: () ->
      this.$el.html this.template(@model.toJSON())
      this

  window.app.views.PaginatedView = Backbone.View.extend
    events:
      'click a.servernext'     : 'nextResultPage'
      'click a.serverprevious' : 'previousResultPage'
      'click a.orderUpdate'    : 'updateSortBy'
      'click a.serverlast'     : 'gotoLast'
      'click a.page'           : 'gotoPage'
      'click a.serverfirst'    : 'gotoFirst'
      'click a.serverpage'     : 'gotoPage'
      'click .serverhowmany a' : 'changeCount'
    tagName: 'aside'
    template: _.template $('#tmpServerPagination').html()

    initialize: () ->
      @collection.on 'reset',  this.render, this
      @collection.on 'change', this.render, this
      this.$el.appendTo '#pagination'

    render: () ->
      html = this.template @collection.info()
      this.$el.html html

    updateSortBy: (e) ->
      e.preventDefault()
      currentSort = $('#sortByField').val()
      @collection.updateOrder currentSort

    nextResultPage: (e) ->
      e.preventDefault()
      @collection.requestNextPage()

    previousResultPage: (e) ->
      e.preventDefault()
      @collection.requestPreviousPage()

    gotoFirst: (e) ->
      e.preventDefault()
      @collection.goTo @collection.information.firstPage

    gotoLast: (e) ->
      e.preventDefault()
      @collection.goTo @collection.information.lastPage

    gotoPage: (e) ->
      e.preventDefault()
      page = $(e.target).text()
      @collection.goTo page

    changeCount: (e) ->
      e.preventDefault()
      per = $(e.target).text()
      @collection.howManyPer per

  window.app.views.AppView = Backbone.View.extend
    el: '#trigramlist'

    initialize: () ->
      tags = @collection

      tags.on 'add',   this.addOne, this
      tags.on 'reset', this.addAll, this
      tags.on 'all',   this.render, this

      tags.pager()

    addAll: () ->
      @collection.each this.addOne

    addOne: (item) ->
      view = new window.app.views.ResultView( model: item )
      $('#trigramlist').append(view.render().el)

    render: () ->

  # Assembling Instances
  window.app.collections.trigramCollection = new window.app.collections.TrigramCollection()
  window.app.views.app = new window.app.views.AppView
    collection: window.app.collections.trigramCollection
  window.app.views.pagination = new window.app.views.PaginatedView
    collection: window.app.collections.trigramCollection

