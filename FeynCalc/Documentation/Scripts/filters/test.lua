

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


