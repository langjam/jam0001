def canonicalize_identifier(value):
  sb = []
  a = ord('a')
  z = ord('z')
  n0 = ord('0')
  n9 = ord('9')
  for c in value.lower():
    ord_c = ord(c)
    if a <= ord_c <= z:
      sb.append(c)
    elif n0 <= ord_c <= n9:
      sb.append(c)
  return ''.join(sb)
