function Header(el)

    if el.level == 3 and (el.identifier == "see-also" or el.identifier == "examples")  then
        local new=el
        new.identifier=""
        return new
    end 

    local secNameRaw = (pandoc.utils.stringify(el.content)):lower()
    secNameRaw = string.gsub(secNameRaw, '%$', 'dollar')
    el.identifier = secNameRaw
    
    if (el.level == 2) then
        local secName = string.gsub(pandoc.utils.stringify(el.content), '%$', '\\$')
        return pandoc.RawBlock('tex','\\hypertarget{' .. el.identifier .. '}{\n\\section{' .. secName .. '}\\label{' .. el.identifier .. '}\\index{' .. secName .. '}}' )
    end       
end


