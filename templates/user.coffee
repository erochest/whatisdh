
$ ->
  $('#rekey').on 'click', ->
    $.post(
      '@{UserRekeyR uid}',
      '',
      ((data) -> $('#key').html(data.key)),
      'json'
    )

