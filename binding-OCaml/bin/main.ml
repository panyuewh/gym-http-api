let () =
  Eio_main.run @@ fun env ->
  let client = Cohttp_eio.Client.make ~https:None env#net in
  Eio.Switch.run @@ fun sw ->
  (** Create a new environment to test an agent. *)
  let instance = Gym_client.env_create "CartPole-v1" ~sw ~client in
  (** Initialize the environment. *)
  let init_observation = Gym_client.env_reset instance in
  (** Select an action according to an observation of the environment. *)
  let random_agent _obs =
    (* sample a random action in the action space of the instance *)
    Gym_client.env_action_space_sample instance in
  (** Simulate the environment using the random agent. *)
  let rec simu obs =
    let action = random_agent obs in
    let step_observation, _, step_done, _ =
      Gym_client.env_step instance action true
    in
    if step_done = true then () else simu step_observation in
  (** Launch the simulation. *)
  simu init_observation;
  (** Close the simulation environment. *)
  Gym_client.env_close instance
