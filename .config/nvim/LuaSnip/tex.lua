return {
  s({trig="beg", snippetType="autosnippet"},
    fmta([[
      \begin{<>}
        <>
      \end{<>}
    ]],
      { i(1), i(2), rep(1) }
    )
  ),
  s({trig="b2", snippetType="autosnippet"},
    fmta([[
      \begin{<>}{<>}
        <>
      \end{<>}
    ]],
      { i(1), i(2), i(3), rep(1) }
    )
  )
}
