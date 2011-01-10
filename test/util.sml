structure UtilTest = TestRunner(struct
  open Test
  open Util

  val tests = [
      Test ("split head empty",
         fn _ => splitHead 0 ([]: int list) = ([], [])),
      Test ("split head none",
         fn _ => splitHead 0 [1, 2, 3] = ([], [1, 2, 3])),
      Test ("split head all",
         fn _ => splitHead 3 [1, 2, 3] = ([1, 2, 3], [])),
      Test ("split head mid",
         fn _ => splitHead 2 [1, 2, 3, 4, 5] = ([1, 2], [3, 4, 5])),
      Test ("split tail empty",
         fn _ => splitTail 0 ([]: int list) = ([], [])),
      Test ("split tail none",
         fn _ => splitTail 0 [1, 2, 3] = ([1, 2, 3], [])),
      Test ("split tail all",
         fn _ => splitTail 3 [1, 2, 3] = ([], [1, 2, 3])),
      Test ("split tail mid",
         fn _ => splitTail 2 [1, 2, 3, 4, 5] = ([1, 2, 3], [4, 5])),
      Test ("max",
         fn _ => max [1, 4, 5, ~3, 2] = 5),
      Test ("lift",
         fn _ => let
              fun f n = List.tabulate (n, (fn n' => n'))
              val lifted = lift f [1, 2, 3]
            in
              lifted = [0, 0, 1, 0, 1, 2]
            end)
  ];

  val continueOnFailure = false
end)
