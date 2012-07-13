
$ ->
  onReindex = (data) ->
    $('#spinner').hide()
    $('#results')
      .show()
      .html("""
        <dl>
          <dt>Documents</dt>    <dd>#{data.document_count}</dd>
          <dt>Tokens</dt>       <dd>#{data.token_count}</dd>
          <dt>Bigrams</dt>      <dd>#{data.bigram_count}</dd>
          <dt>Trigrams</dt>     <dd>#{data.trigram_count}</dd>
          <dt>Elapsed Time</dt> <dd>#{data.elapsed_time}</dd>
        </dl>
        """)

  setTimeout(( ->
    $.post(
      '@{ReindexR}',
      '',
      onReindex,
      'json'
    )),
    5000
  );

