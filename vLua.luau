local vLua = {}

-- Interpreter                                       local function interpret(ast, env)
        env = env or getfenv(0)                      
        local function eval(node, scope)
                if node.type == "Lit" then return node.val
                elseif node.type == "Id" then return scope[node.name]
                elseif node.type == "BinOp" then
                        local l, r = eval(node.l, scope), eval(node.r, scope)
                        if node.op == "+" then return l + r
                        elseif node.op == "-" then return l - r
                        elseif node.op == "*" then return l * r
                        elseif node.op == "/" then return l / r
                        elseif node.op == "%" then return l % r
                        elseif node.op == "^" then return l ^ r
                        elseif node.op == ".." then return tostring(l) .. tostring(r)
                        elseif node.op == "==" then return l == r
                        elseif node.op == "~=" then return l ~= r
                        elseif node.op == "<" then return l < r
                        elseif node.op == ">" then return l > r
                        elseif node.op == "<=" then return l <= r
                        elseif node.op == ">=" then return l >= r
                        elseif node.op == "and" then return l and r
                        elseif node.op == "or" then return l or r
                        end
                elseif node.type == "UnOp" then
                        local op = eval(node.operand, scope)
                        if node.op == "-" then return -op
                        elseif node.op == "#" then return #op
                        elseif node.op == "not" then return not op
                        end
                elseif node.type == "Index" then
                        local obj = eval(node.obj, scope)
                        local idx = eval(node.idx, scope)
                        return obj[idx]
                elseif node.type == "Call" then
                        local fn = eval(node.func, scope)
                        local args = {}
                        for _, arg in ipairs(node.args) do table.insert(args, eval(arg, scope)) end
                        return fn(unpack(args))
                elseif node.type == "MethodCall" then
                        local obj = eval(node.obj, scope)
                        local args = {obj}
                        for _, arg in ipairs(node.args) do table.insert(args, eval(arg, scope)) end
                        return obj[node.method](unpack(args))
                elseif node.type == "Fn" then
                        return function(...)
                                local fscope = setmetatable({}, {__index = scope})
                                local args = {...}
                                for i, p in ipairs(node.params) do fscope[p] = args[i] end
                                if node.vararg then fscope["..."] = {select(#node.params + 1, ...)} end
                                return exec(node.block, fscope)
                        end
                elseif node.type == "Table" then
                        local tbl = {}
                        for _, field in ipairs(node.fields) do
                                if field.key then tbl[eval(field.key, scope)] = eval(field.val, scope)
                                else table.insert(tbl, eval(field.val, scope)) end
                        end
                        return tbl
                elseif node.type == "Vararg" then return unpack(scope["..."] or {})
                end
        end

        local function exec(block, scope)
                scope = scope or setmetatable({}, {__index = env})
                for _, stmt in ipairs(block.stmts) do
                        if stmt.type == "Local" then
                                for i, name in ipairs(stmt.names) do
                                        scope[name] = stmt.vals[i] and eval(stmt.vals[i], scope) or nil
                                end
                        elseif stmt.type == "LocalFn" then
                                scope[stmt.name] = eval(stmt.func, scope)
                        elseif stmt.type == "Assign" then
                                for i, lhs in ipairs(stmt.lhs) do
                                        local val = stmt.rhs[i] and eval(stmt.rhs[i], scope)
                                        if lhs.type == "Id" then scope[lhs.name] = val
                                        elseif lhs.type == "Index" then
                                                local obj = eval(lhs.obj, scope)
                                                local idx = eval(lhs.idx, scope)
                                                obj[idx] = val
                                        end
                                end
                        elseif stmt.type == "FnDecl" then
                                local tbl = scope
                                for i = 1, #stmt.name.parts - 1 do tbl = tbl[stmt.name.parts[i]] end
                                local fname = stmt.name.parts[#stmt.name.parts]
                                if stmt.name.method then
                                        tbl[fname] = tbl[fname] or {}
                                        tbl = tbl[fname]
                                        fname = stmt.name.method
                                        local fn = eval(stmt.func, scope)
                                        tbl[fname] = function(self, ...)
                                                local fscope = setmetatable({}, {__index = scope})
                                                fscope.self = self
                                                local args = {...}
                                                for i, p in ipairs(stmt.func.params) do fscope[p] = args[i] end
                                                return exec(stmt.func.block, fscope)
                                        end
                                else
                                        tbl[fname] = eval(stmt.func, scope)
                                end
                        elseif stmt.type == "If" then
                                if eval(stmt.cond, scope) then exec(stmt.thenb, scope)
                                else
                                        local executed = false
                                        for _, ei in ipairs(stmt.elseifs) do
                                                if eval(ei.cond, scope) then exec(ei.block, scope) executed = true break end
                                        end
                                        if not executed and stmt.elseb then exec(stmt.elseb, scope) end
                                end
                        elseif stmt.type == "While" then
                                while eval(stmt.cond, scope) do exec(stmt.block, scope) end
                        elseif stmt.type == "Repeat" then
                                repeat exec(stmt.block, scope) until eval(stmt.cond, scope)
                        elseif stmt.type == "ForNum" then
                                local start = eval(stmt.start, scope)
                                local finish = eval(stmt.finish, scope)
                                local step = stmt.step and eval(stmt.step, scope) or 1
                                for i = start, finish, step do
                                        local fscope = setmetatable({[stmt.var] = i}, {__index = scope})
                                        exec(stmt.block, fscope)
                                end
                        elseif stmt.type == "ForGen" then
                                local iters = {}
                                for _, it in ipairs(stmt.iters) do table.insert(iters, eval(it, scope)) end
                                local iter, state, var = iters[1], iters[2], iters[3]
                                while true do
                                        local vals = {iter(state, var)}
                                        if vals[1] == nil then break end
                                        var = vals[1]
                                        local fscope = setmetatable({}, {__index = scope})
                                        for i, v in ipairs(stmt.vars) do fscope[v] = vals[i] end
                                        exec(stmt.block, fscope)
                                end
                        elseif stmt.type == "Return" then
                                local rets = {}
                                for _, v in ipairs(stmt.vals) do table.insert(rets, eval(v, scope)) end
                                return unpack(rets)
                        elseif stmt.type == "Do" then exec(stmt.block, scope)
                        elseif stmt.type == "ExprStmt" then eval(stmt.expr, scope)
                        end
                end
        end

        return exec(ast, env)
end

function vLua.Execute(source, scriptType)
        local tokens = tokenize(source)
        local parser = P.new(tokens)
        local ast = parser:block()

        local env = getfenv(2)
        if scriptType == "ModuleScript" then
                local module = {}
                local modenv = setmetatable({}, {__index = function(_, k)
                        if k == "script" then return {ClassName = "ModuleScript"} end
                        return env[k]
                end})
                interpret(ast, modenv)
                return modenv.module or module
        elseif scriptType == "LocalScript" then
                local lenv = setmetatable({}, {__index = function(_, k)
                        if k == "script" then return {ClassName = "LocalScript"} end
                        return env[k]
                end})
                interpret(ast, lenv)
        else
                interpret(ast, env)
        end
end

return vLua
LocalScript"}
                                        end
                                        return env[key]
                                end
                        })
                        interpret(ast, localEnv)
                else
                        interpret(ast, env)
                end
        end)

        task.synchronize()

        if not success then
                error("vLua execution error: " .. tostring(result))
        end

        return result
end

return vLua
