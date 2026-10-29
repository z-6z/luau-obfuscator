local unpack = table.unpack or unpack
local pack = table.pack or function(...)
  return { n = select("#", ...), ... }
end

local VM = {}

local O_MOVE = 0
local O_LOADNIL = 1
local O_LOADBOOL = 2
local O_LOADK = 3
local O_GETGLOBAL = 4
local O_SETGLOBAL = 5
local O_GETTABLE = 6
local O_SETTABLE = 7
local O_SELF = 8
local O_NEWTABLE = 9
local O_ADD = 10
local O_SUB = 11
local O_MUL = 12
local O_DIV = 13
local O_MOD = 14
local O_POW = 15
local O_CONCAT = 16
local O_UNM = 17
local O_NOT = 18
local O_LEN = 19
local O_JMP = 20
local O_TEST = 21
local O_EQ = 22
local O_NEQ = 23
local O_LT = 24
local O_LE = 25
local O_CALL = 26
local O_RETURN = 27
local O_VARARG = 28
local O_FORPREP = 29
local O_FORLOOP = 30
local O_TFORLOOP = 31
local O_NEWCLOSURE = 32
local O_SETLIST = 33
local O_GETUPVAL = 34
local O_SETUPVAL = 35
local O_CLOSE = 36

local run

local function callAny(fnv, args, nargs, env)
  if type(fnv) == "table" and fnv.__vmc then
    return run(fnv.proto, args, nargs, fnv.uprefs, env)
  end
  if type(fnv) == "function" then
    return pack(fnv(unpack(args, 1, nargs)))
  end
  error("attempt to call a " .. type(fnv) .. " value")
end

run = function(proto, args, nargs, uprefs, env)
  local regs = {}
  local nregs = proto[8]
  local top = nregs
  local varargs = {}
  local nva = 0
  local nparams = proto[2]
  local code = proto[4]
  local K = proto[5]
  local protos = proto[6]
  local pc = 0

  for i = 0, nparams - 1 do
    if i < nargs then regs[i] = args[i + 1] end
  end
  if proto[3] then
    for i = nparams, nargs - 1 do
      varargs[i - nparams] = args[i + 1]
      nva = nva + 1
    end
  end

  local function rk(x)
    if x >= 256 then return K[x - 255] end
    return regs[x]
  end

  while true do
    local ins = code[pc + 1]
    local op = ins[1]
    local a = ins[2]
    local b = ins[3]
    local c = ins[4]
    pc = pc + 1

    if op == O_MOVE then
      regs[a] = regs[b]
    elseif op == O_LOADNIL then
      for i = a, a + b do regs[i] = nil end
    elseif op == O_LOADBOOL then
      regs[a] = b ~= 0
      if c ~= 0 then pc = pc + 1 end
    elseif op == O_LOADK then
      regs[a] = K[b + 1]
    elseif op == O_GETGLOBAL then
      regs[a] = env[K[b + 1]]
    elseif op == O_SETGLOBAL then
      env[K[b + 1]] = regs[a]
    elseif op == O_GETTABLE then
      regs[a] = regs[b][rk(c)]
    elseif op == O_SETTABLE then
      regs[a][rk(b)] = rk(c)
    elseif op == O_SELF then
      regs[a + 1] = regs[b]
      regs[a] = regs[b][rk(c)]
    elseif op == O_NEWTABLE then
      regs[a] = {}
    elseif op == O_ADD then
      regs[a] = rk(b) + rk(c)
    elseif op == O_SUB then
      regs[a] = rk(b) - rk(c)
    elseif op == O_MUL then
      regs[a] = rk(b) * rk(c)
    elseif op == O_DIV then
      regs[a] = rk(b) / rk(c)
    elseif op == O_MOD then
      regs[a] = rk(b) % rk(c)
    elseif op == O_POW then
      regs[a] = rk(b) ^ rk(c)
    elseif op == O_CONCAT then
      local s = regs[b]
      for i = b + 1, c do s = s .. regs[i] end
      regs[a] = s
    elseif op == O_UNM then
      regs[a] = -regs[b]
    elseif op == O_NOT then
      regs[a] = not (regs[b] ~= nil and regs[b] ~= false)
    elseif op == O_LEN then
      regs[a] = #regs[b]
    elseif op == O_JMP then
      pc = pc + b
    elseif op == O_TEST then
      if (regs[a] ~= nil and regs[a] ~= false) == (c == 1) then pc = pc + 1 end
    elseif op == O_EQ then
      regs[a] = rk(b) == rk(c)
    elseif op == O_NEQ then
      regs[a] = rk(b) ~= rk(c)
    elseif op == O_LT then
      regs[a] = rk(b) < rk(c)
    elseif op == O_LE then
      regs[a] = rk(b) <= rk(c)
    elseif op == O_CALL then
      local fnv = regs[a]
      local n
      if b == 0 then n = top - (a + 1) else n = b - 1 end
      local t = {}
      for i = 0, n - 1 do t[i + 1] = regs[a + 1 + i] end
      local res = callAny(fnv, t, n, env)
      if c == 0 then
        top = a + res.n
        for i = 0, res.n - 1 do regs[a + i] = res[i + 1] end
      else
        local m = c - 1
        for i = 0, m - 1 do
          if i < res.n then regs[a + i] = res[i + 1] else regs[a + i] = nil end
        end
      end
    elseif op == O_RETURN then
      local res = {}
      if b == 0 then
        res.n = top - a
        for i = 0, res.n - 1 do res[i + 1] = regs[a + i] end
      else
        res.n = b - 1
        for i = 0, res.n - 1 do res[i + 1] = regs[a + i] end
      end
      return res
    elseif op == O_VARARG then
      if b == 0 then
        top = a + nva
        for i = 0, nva - 1 do regs[a + i] = varargs[i] end
      else
        for i = 0, b - 1 do
          if i < nva then regs[a + i] = varargs[i] else regs[a + i] = nil end
        end
      end
    elseif op == O_FORPREP then
      regs[a] = regs[a] - regs[a + 2]
      pc = pc + b
    elseif op == O_FORLOOP then
      regs[a] = regs[a] + regs[a + 2]
      local idx = regs[a]
      local lim = regs[a + 1]
      local step = regs[a + 2]
      if (step >= 0 and idx <= lim) or (step < 0 and idx >= lim) then
        regs[a + 3] = regs[a]
        pc = pc + b
      end
    elseif op == O_TFORLOOP then
      local t = { regs[a + 1], regs[a + 2] }
      local res = callAny(regs[a], t, 2, env)
      for i = 0, b - 1 do
        if i < res.n then regs[a + 3 + i] = res[i + 1] else regs[a + 3 + i] = nil end
      end
      if regs[a + 3] == nil then pc = pc + 1 end
    elseif op == O_NEWCLOSURE then
      local pr = protos[b + 1]
      local puvs = pr[7]
      local ups = {}
      for i = 1, #puvs do
        local uv = puvs[i]
        if uv[1] == 1 then
          ups[i] = { regs, uv[2] }
        else
          ups[i] = uprefs[uv[2] + 1]
        end
      end
      regs[a] = { __vmc = true, proto = pr, uprefs = ups }
    elseif op == O_SETLIST then
      local t = regs[a]
      local n
      if b == 0 then n = top - (a + 1) else n = b end
      local offset = (c - 1) * 50
      for i = 0, n - 1 do t[offset + i + 1] = regs[a + 1 + i] end
    elseif op == O_GETUPVAL then
      local u = uprefs[b + 1]
      regs[a] = u[1][u[2] + 1]
    elseif op == O_SETUPVAL then
      local u = uprefs[b + 1]
      u[1][u[2] + 1] = regs[a]
    elseif op == O_CLOSE then
    else
      error("BAD OPCODE " .. tostring(op))
    end
  end
end

function VM.exec(proto, env)
  env = env or {}
  return run(proto, {}, 0, nil, env)
end

VM.run = run

return VM
