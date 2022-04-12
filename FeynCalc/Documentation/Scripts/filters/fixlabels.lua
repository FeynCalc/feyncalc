function Header(el)
    if el.level == 3 and (el.identifier == "see-also" or el.identifier == "examples")  then
        local new=el
        new.identifier=""
        return new
    end    
end


