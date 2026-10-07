-- Give markdown tables relative column widths so pandoc emits wrapping
-- p{...} columns, and shrink table typography to fit the page.
if FORMAT:match("latex") then
  function Table(tbl)
    local n = #tbl.colspecs
    if n > 0 then
      for i = 1, n do
        local align = tbl.colspecs[i][1]
        -- Last column takes most of the width (Description / sha256).
        local width = (i == n) and 0.58 or ((0.40) / math.max(n - 1, 1))
        if n == 4 and i == 1 then
          width = 0.28
        elseif n == 4 and i == 4 then
          width = 0.42
        elseif n == 4 then
          width = 0.14
        elseif n == 3 and i == 1 then
          width = 0.28
        elseif n == 3 and i == 2 then
          width = 0.12
        elseif n == 3 and i == 3 then
          width = 0.55
        end
        tbl.colspecs[i] = { align, width }
      end
    end
    return {
      pandoc.RawBlock(
        "latex",
        "{\\footnotesize\\setlength{\\tabcolsep}{3pt}"
          .. "\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{0pt}"
      ),
      tbl,
      pandoc.RawBlock("latex", "\\par}")
    }
  end
end
