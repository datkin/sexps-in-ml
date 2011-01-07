structure Util = struct

  fun split limit (element, (n, head, tail)) =
      if n < limit then
        (n + 1, head @ [element], tail)
      else
        (n + 1, head, tail @ [element])

  (* Split a list n elements from the head. *)
  fun splitHead n list =
      let val (_, head, tail) = foldl (split n) (0, [], []) list in
        (head, tail)
      end

  (* Split a list n elements from the tail. *)
  fun splitTail n list = splitHead ((length list) - n) list
end
