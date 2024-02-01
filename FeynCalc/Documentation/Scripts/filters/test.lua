

--~function Link(el)
  --~print(el.tag)
  --~print(el.identifier)
  --~print(el.content.text)
--~end

--    local xxx
--     xxx = PANDOC_STATE.output_file:match("^.+/(.+)$")
--     print("This",xxx)



--return {
--  {
--    Str = function (elem)
--      print (elem.text)
--      if elem.text == "{{helloworld}}" then
--        return pandoc.Emph {pandoc.Str "Hello, World"}
--      else
--        return elem
--      end
--    end,
--  }
--}

--return {
  --{
    --Inline = function (elem)
      ----return pandoc.SmallCaps(elem.c)
      --if elem.tag == "Code" then
        --local new = elem.text
   ----   print pandoc.Str(elem)
----      print(elem.text)
      --new = string.gsub(new, '%[', '[\\allowbreak ')
      ----new = string.gsub(new, '%]', '{]}')
      --new = string.gsub(new, '%{', '\\{\\allowbreak ')
      --new = string.gsub(new, '%}', '\\}')
      --new = string.gsub(new, ',', ',\\ \\allowbreak ')
      ----new = string.gsub(new, '\}', '\}')
      --return pandoc.RawInline('tex', '\\texttt{' .. new  .. '}')
      --end
    --end
  --}
--}


function Header(el)
    if el.level == 3 and (el.identifier == "see-also" or el.identifier == "examples")  then
        local new=el
        new.identifier=""
        return new
    end
    
end


function Pandoc(doc)
    local hblocks = {}
    for i,el in pairs(doc.blocks) do
		print(el.tag)
		print(el.text)
        if (el.t == "Div" and el.classes[1] == "handout") or
           (el.t == "BlockQuote") or
           (el.t == "OrderedList" and el.style == "Example") or
           (el.t == "Para" and #el.c == 1 and el.c[1].t == "Image") or
           (el.t == "Header") then
           table.insert(hblocks, el)
        end
    end
    return pandoc.Pandoc(hblocks, doc.meta)
end


