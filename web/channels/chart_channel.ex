defmodule LiveChart.ChartChannel do
  use Phoenix.Channel

  def join("chart:feed", _message, socket) do
    :timer.send_interval(1_000, :tick)
    {:ok, socket}
  end

  def handle_info(:tick, socket) do
    push(socket, "new_number", %{number: :rand.uniform(99)})
    {:noreply, socket}
  end
end
