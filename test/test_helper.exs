Dynamo.under_test(Haven.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule Haven.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
